-----------------------------------------------------------------------------
-- |
-- Module      :  MisoActionLogger.FFI
-- Copyright   :  (C) 2019 Sviat Chumakov
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  Sviat Chumakov <svchumakov@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module MisoActionLogger.FFI where

import         GHCJS.Types
 
-- | Creates a new inline group in the browser console.
-- This indents following console messages by an additional level,
-- until 'consoleGroupEnd' is called.
foreign import javascript unsafe "console.group.apply(console, $1);"
    consoleGroup 
      :: JSVal  -- ^ a JS array of arguments to print. 
                -- Each one can be either a string to print, a string with CSS styles,
                -- or an Object that will be inspectable in the console.
      -> IO ()

-- | Creates a new inline group in the browser console. Unlike 'consoleGroup', however,
-- the new group is created collapsed. The user will need to use the disclosure button
-- next to it to expand it, revealing the entries created in the group.
foreign import javascript unsafe "console.groupCollapsed.apply(console, $1);"
    consoleGroupCollapsed
      :: JSVal  -- ^ a JS array of arguments to print. 
                -- Each one can be either a string to print, a string with CSS styles,
                -- or an Object that will be inspectable in the console.
      -> IO ()

-- | Exits the current inline group in the browser console. 
foreign import javascript unsafe "console.groupEnd();"
    consoleGroupEnd :: IO ()

-- | Prints to the browser console.
foreign import javascript unsafe "console.log.apply(console, $1);"
    consoleLog
      :: JSVal  -- ^ a JS array of arguments to print. 
                -- Each one can be either a string to print, a string with CSS styles,
                -- or an Object that will be inspectable in the console.
      -> IO ()