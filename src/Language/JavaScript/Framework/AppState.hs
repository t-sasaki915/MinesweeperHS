module Language.JavaScript.Framework.AppState
    ( AppState (..)
    , getAppState
    , setAppState
    , initialiseAppState
    ) where

import           Data.Aeson                                     (FromJSON,
                                                                 ToJSON,
                                                                 decodeStrictText)
import           Data.Aeson.Text                                (encodeToLazyText)
import           Data.Functor                                   ((<&>))
import           Data.Maybe                                     (fromJust)
import           Data.Text                                      (pack)
import           Data.Text.Lazy                                 (unpack)
import           GHC.JS.Prim                                    (JSVal,
                                                                 fromJSString,
                                                                 toJSString)
import           Language.JavaScript.Framework.Internal.Foreign

class (FromJSON a, ToJSON a) => AppState a where
    appStateToJSVal :: a -> JSVal
    appStateToJSVal = toJSString . unpack . encodeToLazyText

    appStateFromJSVal :: JSVal -> a
    appStateFromJSVal = fromJust . decodeStrictText . pack . fromJSString

getAppState :: AppState a => IO a
getAppState = getAppState_ <&> appStateFromJSVal

setAppState :: AppState a => a -> IO ()
setAppState newState = setAppState_ (appStateToJSVal newState)

initialiseAppState :: AppState a => a -> IO ()
initialiseAppState = setAppState
