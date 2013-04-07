module Data.FIT.Parse.Util
  ( computeCRC16
  , convert8
  , convert16
  , convert32
  , crc16
  , toDegree
  , toUTCTime
  ) where
import Data.Array ((!), Array, listArray)
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime))
import Data.Time.Clock.POSIX
  ( POSIXTime
  , posixSecondsToUTCTime
  , utcTimeToPOSIXSeconds
  )
import Data.Word (Word8, Word16, Word32)

table :: Array Int Word16
table = listArray (0, 15)
  [ 0x0000 , 0xCC01 , 0xD801 , 0x1400
  , 0xF001 , 0x3C00 , 0x2800 , 0xE401
  , 0xA001 , 0x6C00 , 0x7800 , 0xB401
  , 0x5000 , 0x9C01 , 0x8801 , 0x4400
  ]

crc16 :: Word8 -> Word16 -> Word16
crc16 w acc = shiftAndTrim crc `xor` valueAt crc `xor` valueAt (shift4 i)
  where
    i = fromIntegral w
    valueAt = (table !) . (.&. 0x0f) . fromIntegral
    shift4 = flip shiftR 4
    shiftAndTrim = (.&. 0x0fff) . shift4
    crc = shiftAndTrim acc `xor` valueAt acc `xor` valueAt i

computeCRC16 :: B.ByteString -> Word16
computeCRC16 = B.foldl (flip crc16) 0

convert8 :: Bool -> Word8 -> Int64
convert8 False i = fromIntegral i
convert8 True  i = fromIntegral (fromIntegral i :: Int8)

convert16 :: Bool -> Word16 -> Int64
convert16 False i = fromIntegral i
convert16 True  i = fromIntegral (fromIntegral i :: Int16)

convert32 :: Bool -> Word32 -> Int64
convert32 False i = fromIntegral i
convert32 True  i = fromIntegral (fromIntegral i :: Int32)

toDegree :: Int64 -> Double 
toDegree = (/ (2 ** 31 / 180)) . fromIntegral

fitEpoch :: POSIXTime
fitEpoch = utcTimeToPOSIXSeconds $ UTCTime (fromGregorian 1989 12 31) 0

toUTCTime :: Int64 -> UTCTime
toUTCTime s = posixSecondsToUTCTime $ fromIntegral s + fitEpoch

