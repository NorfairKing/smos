{-# CFILES window_size.c #-}

module Smos.ASCIInema.WindowSize where

import Data.Bits (shiftL)
import Foreign.C.Types (CInt (..), CLong (..))
import System.Posix.Types (Fd (..))

foreign import ccall "window_size.h c_get_window_size" c_getWindowSize :: Fd -> IO CLong

getWindowSize :: Fd -> IO (Int, Int)
getWindowSize fd = do
  (a, b) <- (`divMod` 65536) `fmap` c_getWindowSize fd
  return (fromIntegral b, fromIntegral a)

foreign import ccall "window_size.h c_set_window_size" c_setWindowSize :: Fd -> CLong -> IO ()

setWindowSize :: Fd -> (Int, Int) -> IO ()
setWindowSize fd (w, h) = do
  let val = (h `shiftL` 16) + w
  c_setWindowSize fd $ fromIntegral val
