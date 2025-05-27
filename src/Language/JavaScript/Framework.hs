module Language.JavaScript.Framework
    ( AppState (..)
    , consoleLog
    , initialiseAppState
    ) where

import           Data.Text                                      (Text, unpack)
import           GHC.JS.Prim                                    (toJSString)
import           Language.JavaScript.Framework.AppState
import           Language.JavaScript.Framework.Internal.Foreign (consoleLog_)

consoleLog :: Text -> IO ()
consoleLog = consoleLog_ . toJSString . unpack
