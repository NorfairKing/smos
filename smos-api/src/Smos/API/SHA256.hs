{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Most of this code came from the 'pantry' package:
--
-- Copyright (c) 2015-2019, Stack contributors
-- All rights reserved.

module Smos.API.SHA256 where

import Control.DeepSeq
import qualified Crypto.Hash as Hash (Digest, SHA256, hash)
import Data.Bits
import Data.ByteArray (ByteArrayAccess (..))
import qualified Data.ByteArray
import qualified Data.ByteArray.Encoding as Mem
import Data.ByteString (ByteString)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Internal as SB
import Data.Data
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Word
import Database.Persist
import Database.Persist.Sql
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

-- | A SHA256 hash, stored in a static size for more efficient
-- memory representation.
--
-- @since 0.1.0.0
newtype SHA256 = SHA256 Bytes32
  deriving (Generic, Eq, NFData, Ord, Typeable)

instance Validity SHA256

-- | Generate a 'SHA256' value by hashing a @ByteString@.
hashBytes :: ByteString -> SHA256
hashBytes = fromDigest . Hash.hash

fromWordsForeign ::
  (ForeignPtr a -> Int -> b) ->
  Int ->
  [Word64] ->
  b
fromWordsForeign wrapper len words0 = unsafePerformIO $ do
  fptr <- SB.mallocByteString len
  withForeignPtr fptr $ \ptr -> do
    let loop _ [] = return ()
        loop off (w : ws) = do
          pokeElemOff (castPtr ptr) off w
          loop (off + 1) ws
    loop 0 words0
  return $ wrapper fptr len

withPeekForeign ::
  (ForeignPtr a, Int, Int) ->
  ((Int -> IO Word64) -> IO b) ->
  IO b
withPeekForeign (fptr, off, len) inner =
  withForeignPtr fptr $ \ptr -> do
    let f off'
          | off' >= len = return 0
          | off' + 8 > len = do
              let loop w64 i
                    | off' + i >= len = return w64
                    | otherwise = do
                        w8 :: Word8 <- peekByteOff ptr (off + off' + i)
                        let w64' = shiftL (fromIntegral w8) (i * 8) .|. w64
                        loop w64' (i + 1)
              loop 0 0
          | otherwise = peekByteOff ptr (off + off')
    inner f

instance Show SHA256 where
  show s = "SHA256 " ++ show (toHexText s)

instance PersistField SHA256 where
  toPersistValue = PersistByteString . toRaw
  fromPersistValue pv = do
    bs <- fromPersistValue pv
    case toStaticExact (bs :: ByteString) of
      Left e -> Left $ tshow e
      Right ss -> pure $ SHA256 ss

instance PersistFieldSql SHA256 where
  sqlType _ = SqlBlob

-- | Convert a 'Hash.Digest' into a 'SHA256'
fromDigest :: Hash.Digest Hash.SHA256 -> SHA256
fromDigest digest =
  case toStaticExact (Data.ByteArray.convert digest :: ByteString) of
    Left e -> error $ "Impossible failure in fromDigest: " ++ show (digest, e)
    Right x -> SHA256 x

-- | Convert a 'SHA256' into a base16-encoded SHA256 hash.
toHexText :: SHA256 -> Text
toHexText ss =
  case TE.decodeUtf8' $ toHexBytes ss of
    Left e -> error $ "Impossible failure in staticSHA256ToText: " ++ show (ss, e)
    Right t -> t

-- | Convert a 'SHA256' into a base16-encoded SHA256 hash.
toHexBytes :: SHA256 -> ByteString
toHexBytes (SHA256 x) = Mem.convertToBase Mem.Base16 x

-- | Convert a 'SHA256' into a raw binary representation.
toRaw :: SHA256 -> ByteString
toRaw (SHA256 x) = Data.ByteArray.convert x

--  Static bytes

newtype Bytes8 = Bytes8 Word64
  deriving (Validity, Eq, Ord, Generic, NFData)

data Bytes16 = Bytes16 !Bytes8 !Bytes8
  deriving (Validity, Eq, Ord, Generic, NFData)

data Bytes32 = Bytes32 !Bytes16 !Bytes16
  deriving (Validity, Eq, Ord, Generic, NFData)

data StaticBytesException
  = NotEnoughBytes
  | TooManyBytes
  deriving (Show, Typeable)

class DynamicBytes dbytes where
  lengthD :: dbytes -> Int

  -- | Yeah, it looks terrible to use a list here, but fusion should
  -- kick in
  withPeekD :: dbytes -> ((Int -> IO Word64) -> IO a) -> IO a

  -- | May throw a runtime exception if invariants are violated!
  fromWordsD :: Int -> [Word64] -> dbytes

instance DynamicBytes ByteString where
  lengthD = SB.length
  fromWordsD = fromWordsForeign (\fptr len -> SB.fromForeignPtr fptr 0 len)
  withPeekD = withPeekForeign . SB.toForeignPtr

class StaticBytes sbytes where
  lengthS :: proxy sbytes -> Int -- use type level literals instead?
  -- difference list

  toWordsS :: sbytes -> [Word64] -> [Word64]
  usePeekS :: Int -> (Int -> IO Word64) -> IO sbytes

instance StaticBytes Bytes8 where
  lengthS _ = 8
  toWordsS (Bytes8 w) = (w :)
  usePeekS off f = Bytes8 <$> f off

instance StaticBytes Bytes16 where
  lengthS _ = 16
  toWordsS (Bytes16 b1 b2) = toWordsS b1 . toWordsS b2
  usePeekS off f = Bytes16 <$> usePeekS off f <*> usePeekS (off + 8) f

instance StaticBytes Bytes32 where
  lengthS _ = 32
  toWordsS (Bytes32 b1 b2) = toWordsS b1 . toWordsS b2
  usePeekS off f = Bytes32 <$> usePeekS off f <*> usePeekS (off + 16) f

instance ByteArrayAccess Bytes32 where
  length _ = 32
  withByteArray = withByteArrayS

withByteArrayS :: (StaticBytes sbytes) => sbytes -> (Ptr p -> IO a) -> IO a
withByteArrayS sbytes = withByteArray (fromStatic sbytes :: ByteString)

toStaticExact ::
  forall dbytes sbytes.
  (DynamicBytes dbytes, StaticBytes sbytes) =>
  dbytes ->
  Either StaticBytesException sbytes
toStaticExact dbytes =
  case compare (lengthD dbytes) (lengthS (Nothing :: Maybe sbytes)) of
    LT -> Left NotEnoughBytes
    GT -> Left TooManyBytes
    EQ -> Right (toStaticPadTruncate dbytes)

toStaticPadTruncate ::
  (DynamicBytes dbytes, StaticBytes sbytes) =>
  dbytes ->
  sbytes
toStaticPadTruncate dbytes = unsafePerformIO (withPeekD dbytes (usePeekS 0))

fromStatic ::
  forall dbytes sbytes.
  (DynamicBytes dbytes, StaticBytes sbytes) =>
  sbytes ->
  dbytes
fromStatic = fromWordsD (lengthS (Nothing :: Maybe sbytes)) . ($ []) . toWordsS

tshow :: (Show a) => a -> Text
tshow = T.pack . show
