module Language.JavaScript.Wrapper
    ( Element (..)
    , ElementType (..)
    , EventType (..)
    , consoleLog
    , alert
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
    , addEventListenerNoState
    , removeAllChildren
    , getURLSearchParam
    , randomInt
    , refreshPage
    ) where

import           Data.Text                                    (Text, pack,
                                                               unpack)
import           GHC.JS.Prim                                  (fromJSString,
                                                               isNull,
                                                               toJSString)
import           Language.JavaScript.Wrapper.ElementWrapper
import           Language.JavaScript.Wrapper.EventWrapper
import           Language.JavaScript.Wrapper.Internal.Foreign (alert_,
                                                               consoleLog_,
                                                               getURLSearchParam_,
                                                               randomInt_,
                                                               refreshPage_)

consoleLog :: Text -> IO ()
consoleLog = consoleLog_ . toJSString . unpack

alert :: Text -> IO ()
alert = alert_ . toJSString . unpack

getURLSearchParam :: Text -> IO (Maybe Text)
getURLSearchParam paramName =
    getURLSearchParam_ (toJSString $ unpack paramName) >>= \rawParam ->
        if isNull rawParam then pure Nothing
                           else pure $ Just (pack $ fromJSString rawParam)

randomInt :: Int -> Int -> IO Int
randomInt = randomInt_

refreshPage :: IO ()
refreshPage = refreshPage_
