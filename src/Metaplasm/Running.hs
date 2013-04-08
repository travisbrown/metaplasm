{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, QuasiQuotes #-}
module Metaplasm.Running where
import Control.Applicative ((<$>))
import Control.Exception (Exception, throw)
import Data.Aeson.Types (toJSON)
import Data.FIT.Parse (parseFile)
import qualified Data.Map as M
import Data.List (elemIndex, intersperse, sortBy)
import Data.Monoid (Sum, mappend, mempty)
import Data.Ord (comparing)
import Data.Running
import Data.Text (Text, pack)
import Data.Time.Clock (diffUTCTime)
import Data.Time.Format (formatTime)
import Data.Typeable (Typeable)
import Hakyll.Core.Compiler (Compiler, getUnderlying)
import Hakyll.Core.Identifier (Identifier, toFilePath)
import Hakyll.Core.Identifier.Pattern (Pattern)
import Hakyll.Core.Item (Item, itemIdentifier)
import Hakyll.Core.Metadata (getMatches)
import Hakyll.Core.Routes (Routes, customRoute)
import Hakyll.Core.Rules (Rules, preprocess)
import Hakyll.Web.Template.Context (Context, constField)
import System.Locale (defaultTimeLocale)
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Julius (julius, rawJS, renderJavascriptUrl)
import Text.Printf (printf)

data RunningException = MissingIdentifierException
  deriving (Show, Typeable)

instance Exception RunningException

type Tracks = M.Map Identifier [PointRecord]

fitTracks :: Pattern -> Rules Tracks
fitTracks pattern = do
  matches <- getMatches pattern
  parsed <- preprocess $ mapM (parseFile . toFilePath) matches
  return . M.fromList $ zip matches parsed

fitId :: Tracks -> Identifier -> [String]
fitId tracks i = 
  maybe (throw MissingIdentifierException)
  pointsToPath
  (M.lookup i tracks)
  where
    pointsToPath :: [PointRecord] -> [String]
    pointsToPath points = date ++ [printf "%02d" $ counter + 1]
      where
        counter = maybe (throw MissingIdentifierException) id
          . elemIndex i
          . map fst
          . sortBy (comparing snd)
          . M.toList
          $ M.filter ((== date) . pointDate . last) tracks 
        date = pointDate $ last points
        pointDate point = [pointY point, pointM point, pointD point]
        pointY = formatTime defaultTimeLocale "%Y" .  pointTime
        pointM = formatTime defaultTimeLocale "%m" .  pointTime
        pointD = formatTime defaultTimeLocale "%d" .  pointTime

fitPath :: Tracks -> Identifier -> FilePath
fitPath tracks i =
  "running/" ++ concat (intersperse "/" $ fitId tracks i) ++ "/index.html"

fitRoute :: Tracks -> Routes
fitRoute = customRoute . fitPath

gMapsApiScript = renderHtml 
  $ H.script
  ! A.src "https://maps.googleapis.com/maps/api/js?v=3.exp&sensor=false"
  $ mempty

fitCtx :: [PointRecord] -> Context String -> Context String
fitCtx points ctx =
  constField "gMapsApiScript" gMapsApiScript
  `mappend` constField "date" (formatTime defaultTimeLocale "%e %B %Y" . pointTime . last $ points)
  `mappend` constField "datetime" (formatTime defaultTimeLocale "%Y-%m-%d" . pointTime . last $ points)
  `mappend` constField "time" (printf "%d minutes" $ totalMinutes points)
  `mappend` constField "speed" (printf "%0.2f miles per hour" $ averageSpeed points)
  `mappend` constField "title" (printf "%0.2f miles" $ totalMiles points)
  `mappend` ctx

fitBody :: Tracks -> [PointRecord] -> Compiler String
fitBody tracks points = do
  path <- fitId tracks <$> getUnderlying
  let fitDivId = pack . concat $ intersperse "-" $ "run" : path
  let fitInitName = pack . concat $ intersperse "_" $ "init" : path
  let (centerX, centerY) = simpleCentroid points
  let coords = pack . concat . intersperse "," $ map (\p -> printf "new google.maps.LatLng(%f, %f)" (pointLat p) (pointLng p)) points
  let script = renderJavascriptUrl (\_ _ -> undefined) [julius|
      function #{rawJS fitInitName}() {
        var centroid = new google.maps.LatLng(#{toJSON centerX}, #{toJSON centerY});
        var mapOptions = {
          zoom: 14,
          center: centroid,
          mapTypeId: google.maps.MapTypeId.ROADMAP
        };

        var map = new google.maps.Map(document.getElementById(#{toJSON fitDivId}), mapOptions);

        var trackCoords = [
          #{rawJS coords}
        ];
        var track = new google.maps.Polyline({
          path: trackCoords,
          strokeColor: '#C0362C',
          strokeOpacity: 1.0,
          strokeWeight: 4
        });

        track.setMap(map);
      }

      google.maps.event.addDomListener(window, 'load', #{rawJS fitInitName});
    |]
  return . renderHtml $ H.script (toHtml script) `mappend` (H.div ! A.id (toValue fitDivId) ! A.class_ "run-map") mempty

fitRecentFirst :: Tracks -> [Item a] -> [Item a]
fitRecentFirst tracks = reverse . sortBy (comparing $ fitId tracks . itemIdentifier)

metersToMiles :: Double -> Double
metersToMiles m = m * 0.000621371

totalMinutes :: [PointRecord] -> Int
totalMinutes points = (round $ diffUTCTime end start) `div` 60
  where
    start = pointTime $ head points
    end = pointTime $ last points

totalMiles :: [PointRecord] -> Double
totalMiles = metersToMiles . pointDistance . last

averageSpeed :: [PointRecord] -> Double
averageSpeed points = totalMiles points / ((fromRational . toRational $ diffUTCTime end start) / 3600)
  where
    start = pointTime $ head points
    end = pointTime $ last points

simpleCentroid :: [PointRecord] -> (Double, Double)
simpleCentroid points = 
  ((/ size) . sum $ map pointLat points, (/ size) . sum $ map pointLng points)
  where
    size = fromIntegral $ length points

