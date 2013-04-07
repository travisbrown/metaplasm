{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module Metaplasm.Running where
import Control.Exception (Exception, throw)
import Data.FIT.Parse (parseFile)
import qualified Data.Map as M
import Data.List (elemIndex, sortBy)
import Data.Ord (comparing)
import Data.Running
import Data.Time.Format (formatTime)
import Data.Typeable (Typeable)
import Hakyll.Core.Identifier (Identifier, toFilePath)
import Hakyll.Core.Identifier.Pattern (Pattern)
import Hakyll.Core.Metadata (getMatches)
import Hakyll.Core.Routes (Routes, customRoute)
import Hakyll.Core.Rules (Rules, preprocess)
import System.Locale (defaultTimeLocale)
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

fitPath :: Tracks -> Identifier -> FilePath
fitPath tracks i =
  maybe (throw MissingIdentifierException)
  pointsToPath
  (M.lookup i tracks)
  where
    pointsToPath :: [PointRecord] -> FilePath
    pointsToPath points = printf "%s/%02d/index.kml" datePath $ counter + 1
      where
        counter = maybe (throw MissingIdentifierException) id
          . elemIndex i
          . map fst
          . sortBy (comparing snd)
          . M.toList
          $ M.filter ((== datePath) . pointToDatePath . last) tracks 
        datePath = pointToDatePath $ last points
        pointToDatePath = formatTime defaultTimeLocale "%Y/%m/%d" .  pointTime

fitRoute :: Tracks -> Routes
fitRoute = customRoute . fitPath


