module Language.JavaScript.Framework.WrappedForeignFunction
    ( consoleLog
    , getAppState
    , setAppState
    , initialiseAppState
    ) where

import           Data.Functor                                   ((<&>))
import           Data.Text                                      (Text, unpack)
import           GHC.JS.Prim                                    (toJSString)
import           Language.JavaScript.Framework.AppState         (AppState (..),
                                                                 appStateToJSVal)
import           Language.JavaScript.Framework.Internal.Foreign

consoleLog :: Text -> IO ()
consoleLog = consoleLog_ . toJSString . unpack

getAppState :: AppState a => IO a
getAppState = getAppState_ <&> appStateFromJSVal

setAppState :: AppState a => a -> IO ()
setAppState newState = setAppState_ (appStateToJSVal newState)

initialiseAppState :: AppState a => a -> IO ()
initialiseAppState = setAppState
