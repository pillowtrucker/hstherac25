module Main (main) where

-- The VT100 console tests are written in C (vt100/test/tests.c); this only runs them.

import Foreign.C.Types (CInt (..))
import System.Exit (exitFailure)

foreign import ccall safe "vt_run_tests" runTests :: IO CInt

main :: IO ()
main = do
  failures <- runTests
  if failures == 0 then pure () else exitFailure
