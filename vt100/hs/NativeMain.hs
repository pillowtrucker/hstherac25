module Main (main) where

-- The Therac-25 console program on a real terminal (vt100/src/native_tty.c).

import Foreign.C.Types (CInt (..))
import System.Environment (getArgs)
import System.Exit (ExitCode (..), die, exitWith)

foreign import ccall safe "therac_native_main" nativeMain :: CInt -> IO CInt

main :: IO ()
main = do
  args <- getArgs
  baud <- case args of
    [] -> pure 9600
    ["--baud", n] | [(b, "")] <- reads n, b >= 0 -> pure b
    _ -> die "usage: therac-vt100 [--baud N]   (default 9600; 0 = as fast as the terminal takes it)"
  r <- nativeMain (fromIntegral (baud :: Int))
  exitWith (if r == 0 then ExitSuccess else ExitFailure (fromIntegral r))
