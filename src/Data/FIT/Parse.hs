{-# LANGUAGE DoAndIfThenElse, OverloadedStrings, TupleSections #-}
module Data.FIT.Parse
  ( parseBytes
  , parseFile
) where
import Control.Applicative ((*>), (<$>), (<*>))
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Binary.Get as G
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.FIT.Parse.Format (fieldGetter, isSignedField)
import Data.FIT.Parse.Util
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (catMaybes, maybeToList)
import Data.Running
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Format ()
import Data.Word (Word8, Word16)

data Data = Data
  { dataId :: Word16
  , dataFields :: [(Word8, Int64)]
  } deriving (Eq, Ord, Show)

type Definition = [(Word8, G.Get Int64)]
type Definitions = [((Word8, Word16), Definition)]

lGetWord8 :: MonadTrans t => t G.Get Word8
lGetWord8 = lift G.getWord8

parseDefinition :: ExceptT Text G.Get (Word16, Definition)
parseDefinition =
  do
    _      <- lift (G.skip 2)
    global <- lift G.getWord16le
    count  <- fromIntegral <$> lGetWord8
    def <- replicateM count $ parseField global
    return (global, def)
  where
    action :: G.Get (Either Text (G.Get Int64))
    action = fieldGetter <$> G.getWord8 <*> (isSignedField <$> G.getWord8)

    parseField :: Word16 -> ExceptT Text G.Get (Word8, G.Get Int64)
    parseField global =
      (,) <$> lGetWord8 <*> (liftEither =<< lift action)

-- | Note that we're not actually using the either monad here.
parseDataFields :: Definition -> ExceptT Text G.Get [(Word8, Int64)]
parseDataFields = mapM readField 
  where
    readField (fieldId, action) = (,) fieldId <$> lift action

parseRecord :: StateT Definitions (ExceptT Text G.Get) (Maybe Data)
parseRecord = do
  header <- lift lGetWord8
  let recordId = header .&. 0xf
  if (header .&. 0x40) == 0x40
  then (lift parseDefinition >>= \(global, def) -> modify (((recordId, global), def) :)) *> return Nothing
  else do
    ((_, global), definition) <- (maybe typeError return . find ((== recordId) . fst . fst)) =<< get
    Just . Data global <$> lift (parseDataFields definition)
    where typeError = lift $ liftEither $ Left "Unknown data message type!"

parseRecords :: Int -> StateT Definitions (ExceptT Text G.Get) [Data]
parseRecords size = do
  bytesRead <- lift . lift $ fromIntegral <$> G.bytesRead
  if bytesRead == size
  then return []
  else (++) <$> (maybeToList <$> parseRecord) <*> parseRecords size

pointRecords :: [Data] -> [PointRecord]
pointRecords = catMaybes . map selectPointRecord
  where
    selectPointRecord (Data 20 fields) = PointRecord
      <$> (toUTCTime <$> lookup 253 fields)
      <*> (curry Coord <$> (toDegree <$> lookup 0 fields) <*> (toDegree <$> lookup 1 fields))
      <*> ((/ 100) . fromIntegral <$> lookup 5 fields)
      <*> ((/ 1000) . fromIntegral <$> lookup 5 fields)
    selectPointRecord _ = Nothing 

parseFIT :: Word16 -> ExceptT Text G.Get [PointRecord]
parseFIT crc = do
  headerSize <- fromIntegral <$> lGetWord8
  protocolVersion <- (\w -> (shiftR (240 .&. w) 4, 0x0f .&. w)) <$> lGetWord8
  profileVersion <- lift G.getWord16le
  contentSize <- fromIntegral <$> lift G.getWord32le
  ext <- decodeUtf8 <$> lift (G.getByteString 4)
  when (ext /= ".FIT") $ liftEither $ Left "Invalid file header!"
  _ <- lift . G.skip $ headerSize - 12
  (records, defs) <- runStateT (parseRecords $ contentSize + headerSize) []
  check <- (crc ==) <$> lift G.getWord16le
  unless check $ liftEither $ Left "File corrupted (invalid checksum)!"
  return $ pointRecords records

-- | Parse a lazy byte string representing a FIT file. Note that we force
-- evaluation of most of the string immediately, to compute the checksum. This
-- isn't necessarily ideal, but these files tend to be small.
parseBytes :: BL.ByteString -> Either Text [PointRecord]
parseBytes contents = G.runGet (runExceptT $ parseFIT crc) contents
  where
    crc = computeCRC16 $ BL.take (BL.length contents - 2) contents

-- | A convenience function that parses a file and ignores errors. 
parseFile :: FilePath -> IO [PointRecord] 
parseFile file = do
  contentsStrict <- B.readFile file
  let contents = BL.fromStrict contentsStrict
  let crc = computeCRC16 $ BL.take (BL.length contents - 2) contents
  let Right parsed = G.runGet (runExceptT $ parseFIT crc) contents
  return parsed

