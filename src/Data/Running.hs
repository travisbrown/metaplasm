module Data.Running
  ( PointRecord (PointRecord)
  , pointDistance
  , pointLat
  , pointLng
  , pointTime
  , pointSpeed
) where
import Data.Time.Clock (UTCTime)
import Data.Time.Format ()
import Statistics.Sample (mean, stdDev)

data PointRecord = PointRecord
  { pointTime :: UTCTime
  , pointLat :: Double
  , pointLng :: Double
  , pointDistance :: Double
  , pointSpeed :: Double
  } deriving (Eq, Ord, Show)



