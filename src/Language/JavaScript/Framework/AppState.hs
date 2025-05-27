module Language.JavaScript.Framework.AppState (AppState (..)) where

import           Data.Aeson      (FromJSON, ToJSON, decodeStrictText)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Maybe      (fromJust)
import           Data.Text       (pack)
import           Data.Text.Lazy  (unpack)
import           GHC.JS.Prim     (JSVal, fromJSString, toJSString)

class (FromJSON a, ToJSON a) => AppState a where
    appStateToJSVal :: a -> JSVal
    appStateToJSVal = toJSString . unpack . encodeToLazyText

    appStateFromJSVal :: JSVal -> a
    appStateFromJSVal = fromJust . decodeStrictText . pack . fromJSString
