module Smos.Server.DB.Compressed where

import Codec.Compression.Zstd as Zstd
import Data.ByteString
import qualified Data.ByteString as SB
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word
import Database.Persist
import Database.Persist.Sql

newtype Compressed = Compressed {compressedByteString :: ByteString}
  deriving (Show, Eq)

compressByteString :: Int -> ByteString -> Compressed
compressByteString level = Compressed . Zstd.compress level

decompressByteString :: Compressed -> Either Text ByteString
decompressByteString (Compressed bs) = case Zstd.decompress bs of
  Skip -> Right SB.empty
  Error s -> Left (T.pack s)
  Decompress res -> Right res

decompressByteStringOrErrorMessage :: Compressed -> ByteString
decompressByteStringOrErrorMessage c = case decompressByteString c of
  Left err -> TE.encodeUtf8 err
  Right res -> res

-- The fromIntegral is safe because it is Int -> Word64
compressedSize :: Compressed -> Word64
compressedSize = fromIntegral . SB.length . compressedByteString

instance PersistFieldSql Compressed where
  sqlType Proxy = sqlType (Proxy :: Proxy ByteString)

instance PersistField Compressed where
  toPersistValue = toPersistValue . compressedByteString
  fromPersistValue = fmap Compressed . fromPersistValue
