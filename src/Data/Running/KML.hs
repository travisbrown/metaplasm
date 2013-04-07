{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Data.Running.KML
  ( trailToKML
  ) where
import Data.Running
import Text.XML
import Text.Hamlet.XML
import qualified Data.Map as M
import Data.Text (Text, pack)

trailToKML :: [PointRecord] -> Document
trailToKML points = Document (Prologue [] Nothing []) root [] 
  where
    indexedPoints = zip [0..] points
    root = Element "{http://www.opengis.net/kml/2.2}kml" M.empty [xml|
      <{http://www.opengis.net/kml/2.2}Document> 
        <{http://www.opengis.net/kml/2.2}name>
          Run
        <{http://www.opengis.net/kml/2.2}Style id="myDefaultStyle">
          <{http://www.opengis.net/kml/2.2}LineStyle>
            <{http://www.opengis.net/kml/2.2}color>ff0000ff
            <{http://www.opengis.net/kml/2.2}width>15
        $forall indexedPoint <- indexedPoints
          ^{renderPoint indexedPoint}
        <{http://www.opengis.net/kml/2.2}Placemark id="path">
          <{http://www.opengis.net/kml/2.2}name>Path
          <{http://www.opengis.net/kml/2.2}LineString>
            <{http://www.opengis.net/kml/2.2}coordinates>
              $forall point <- points
                #{showCoord point}
    |]

renderPoint :: (Int, PointRecord) -> [Node]
renderPoint (i, PointRecord _ lat lng _ speed) =
  [xml|
    <{http://www.opengis.net/kml/2.2}Placemark id="point-#{pack $ show i}">
      <{http://www.opengis.net/kml/2.2}name>Point #{pack $ show i}
      <{http://www.opengis.net/kml/2.2}description>
        Speed: #{pack $ show $ speed}
      <{http://www.opengis.net/kml/2.2}Point>
        <{http://www.opengis.net/kml/2.2}coordinates>
          #{pack $ show $ lng},#{pack $ show $ lat}
  |]

showCoord :: PointRecord -> Text
showCoord (PointRecord _ lat lng _ _) =
  pack $ show lng ++ "," ++ show lat ++ "\n"

