{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  MisoActionLogger
-- Copyright   :  (C) 2019 Sviat Chumakov
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  Sviat Chumakov <svchumakov@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module MisoActionLogger
  ( mkActionLogger
  , defaultActionLogger
  , defaultGroupFormat
  , defaultStateFormat
  , defaultActionFormat
  , ActionLoggerOptions
  ) where

import           Data.Aeson
import           Miso (Effect(..))
import           Miso.String hiding (head, words)
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure

import           MisoActionLogger.FFI

-- | Configuration for mkActionLogger.
data ActionLoggerOptions action model = ActionLoggerOptions
  { groupFormat         :: action -> IO JSVal
   -- ^ Function to format the log group title
  , previousStateFormat :: model  -> IO JSVal
   -- ^ Function to format the previous state
  , actionFormat        :: action -> IO JSVal
   -- ^ Function to format the action
  , nextStateFormat     :: model  -> IO JSVal
   -- ^ Function to format the next state
  }

-- | Create an action logger with custom settings.
-- Returns a logger that wraps around your 'update' function when you construct a Miso app.
mkActionLogger
  :: (Show action, ToJSON model)
  => ActionLoggerOptions action model
  -> (action -> model -> Effect action model)
  -> action -> model -> Effect action model
mkActionLogger ActionLoggerOptions{..} update action oldModel = Effect newModel (logSub : subs)
    where
      Effect newModel subs = update action oldModel
      logSub _ = do
         groupVal  <- groupFormat action
         oldVal    <- previousStateFormat oldModel
         actionVal <- actionFormat action
         newVal    <- nextStateFormat newModel

         consoleGroupCollapsed groupVal
         consoleLog oldVal
         consoleLog actionVal
         consoleLog newVal
         consoleGroupEnd

-- | Action logger with default settings.
-- Wraps around your 'update' function when you construct a Miso app.
defaultActionLogger
  :: (Show action, ToJSON model)
  => (action -> model -> Effect action model)
  -> action -> model -> Effect action model
defaultActionLogger = mkActionLogger ActionLoggerOptions
  { groupFormat = defaultGroupFormat
  , previousStateFormat = defaultStateFormat defaultPreviousStateLabel defaultPreviousStateStyle
  , actionFormat = defaultActionFormat
  , nextStateFormat = defaultStateFormat defaultNextStateLabel defaultNextStateStyle
  }

defaultPreviousStateLabel :: MisoString
defaultPreviousStateLabel = "%cprev state %c"

defaultNextStateLabel :: MisoString
defaultNextStateLabel = "%cnext state %c"

defaultPreviousStateStyle :: MisoString
defaultPreviousStateStyle = "color: #9E9E9E; font-weight: bold;"

defaultNextStateStyle :: MisoString
defaultNextStateStyle = "color: #4CAF50; font-weight: bold;"

defaultGroupFormat :: Show action => action -> IO JSVal
defaultGroupFormat action =
  toJSVal
    [ "%caction %c" <> ((ms . head . words . show) action)
    , "color: gray; font-weight: lighter;"
    , ""
    ]

defaultStateFormat :: ToJSON model => MisoString -> MisoString -> model -> IO JSVal
defaultStateFormat label style model = do
  modelVal <- toJSVal (toJSON model) 
  toJSVal
    [ pToJSVal label
    , pToJSVal style
    , pToJSVal ("" :: MisoString)
    , modelVal
    ]

defaultActionFormat :: Show action => action -> IO JSVal
defaultActionFormat action =
  toJSVal
    [ pToJSVal ("%caction %c" :: MisoString)
    , pToJSVal ("color: #03A9F4; font-weight: bold;" :: MisoString)
    , pToJSVal ("" :: MisoString)
    , pToJSVal $ show action
    ]