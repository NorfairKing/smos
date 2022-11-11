module Codec.Compression.Zstd.Extended (defaultCLevel) where

import Foreign.C.Types (CInt (..))

-- | Returns the default compression level supported by the zstd library.
foreign import ccall unsafe "ZSTD_defaultCLevel"
  c_defaultCLevel :: CInt

-- | The default compression level supported by the zstd library.
defaultCLevel :: Int
defaultCLevel = fromIntegral c_defaultCLevel
