module Language.JavaScript.Wrapper
    ( Element (..)
    , ElementType (..)
    , EventType (..)
    , consoleLog
    , createElement
    , createTextNode
    , setElementId
    , setElementClassName
    , setElementInnerHtml
    , setElementValue
    , setIsElementSelected
    , appendChild
    , getElementById
    , addEventListener
    , addEventListenerWithState
    , getURLSearchParam
    ) where

import           Data.Text                                    (Text, pack,
                                                               unpack)
import           GHC.JS.Prim                                  (fromJSString,
                                                               isNull,
                                                               toJSString)
import           Language.JavaScript.Wrapper.ElementWrapper
import           Language.JavaScript.Wrapper.EventWrapper
import           Language.JavaScript.Wrapper.Internal.Foreign (consoleLog_,
                                                               getURLSearchParam_)

consoleLog :: Text -> IO ()
consoleLog = consoleLog_ . toJSString . unpack

getURLSearchParam :: Text -> IO (Maybe Text)
getURLSearchParam paramName =
    getURLSearchParam_ (toJSString $ unpack paramName) >>= \rawParam ->
        if isNull rawParam then return Nothing
                           else return $ Just (pack $ fromJSString rawParam)
