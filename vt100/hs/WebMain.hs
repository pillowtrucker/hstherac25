module Main (main) where

-- The browser build is a wasm reactor: the page calls the C entry points in
-- vt100/src/web_main.c, which reach the simulator through csrc/Therac.h.
--
-- This module passes a JavaScript value through the JavaScript FFI, which is what makes the
-- linker include GHC's JSFFI runtime support. That support starts the Haskell runtime from
-- _initialize and makes threadDelay wait with setTimeout instead of blocking the page (GHC's
-- Note [threadDelay on wasm]), so the simulator's Treat, housekeeper and Ptime tasks run on the
-- browser's event loop, unchanged.

import GHC.Wasm.Prim (JSVal, freeJSVal)

foreign import javascript unsafe "console.info('hstherac25: Therac-25 simulator running'); return globalThis;"
  js_hello :: IO JSVal

foreign export javascript "therac_boot sync" boot :: IO ()

boot :: IO ()
boot = js_hello >>= freeJSVal

main :: IO ()
main = pure ()
