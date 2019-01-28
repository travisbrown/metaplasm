{-# LANGUAGE TypeFamilies #-}
module Data.Running
  ( Coord (Coord)
  , PointRecord (PointRecord)
  , pointDistance
  , pointCoord
  , pointLat
  , pointLng
  , pointTime
  , pointSpeed
) where
import Data.Time.Clock (UTCTime)
import Data.Time.Format ()
import Statistics.Sample (mean, stdDev)
import Data.AdditiveGroup
import Data.VectorSpace

newtype Coord = Coord (Double, Double) deriving (Eq, Ord, Show)

data PointRecord = PointRecord
  { pointTime :: UTCTime
  , pointCoord :: Coord
  , pointDistance :: Double
  , pointSpeed :: Double
  } deriving (Eq, Ord, Show)

pointLat (PointRecord _ (Coord (a, _)) _ _) = a
pointLng (PointRecord _ (Coord (_, b)) _ _) = b

instance AdditiveGroup Coord where
  zeroV = Coord (0, 0)
  Coord (a, b) ^+^ Coord (c, d) = Coord (a + c, b + d)
  negateV (Coord (a, b)) = Coord (-a, -b)

instance InnerSpace Coord where
  Coord (a, b) <.> Coord (c, d) = a * c + b * d

instance VectorSpace Coord where
  type Scalar Coord = Double
  s *^ Coord (a, b) = Coord (s * a, s * b)


--instance VectorSpace PointRecord where
--  type Scalar =  

