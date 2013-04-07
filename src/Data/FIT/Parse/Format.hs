{-# LANGUAGE OverloadedStrings #-}
module Data.FIT.Parse.Format
  ( fieldGetter
  , isSignedField
  ) where
import Control.Applicative ((<$>))
import qualified Data.Binary.Get as G
import Data.Int (Int64)
import Data.FIT.Parse.Util (convert8, convert16, convert32)
import Data.Text
import Data.Word (Word8)

fieldGetter :: Word8 -> Bool -> Either Text (G.Get Int64)
fieldGetter 1 signed = Right $ convert8  signed <$> G.getWord8
fieldGetter 2 signed = Right $ convert16 signed <$> G.getWord16le
fieldGetter 4 signed = Right $ convert32 signed <$> G.getWord32le
fieldGetter _ _ = Left "Invalid field size!"

isSignedField :: Word8 -> Bool
isSignedField 0x01 = True
isSignedField 0x83 = True
isSignedField 0x85 = True
isSignedField _ = False

