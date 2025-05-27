module Language.JavaScript.Wrapper.WrappedForeignFunction
    ( consoleLog
    , createElement
    , setElementId
    , setElementClassName
    , appendChildToBody
    ) where

import           Data.Text                               (Text, unpack)
import           GHC.JS.Prim                             (JSVal, toJSString)
import           Language.JavaScript.Foreign
import           Language.JavaScript.Wrapper.ElementType (ElementType,
                                                          elementName)

consoleLog :: Text -> IO ()
consoleLog = consoleLog_ . toJSString . unpack

createElement :: ElementType -> IO JSVal
createElement = createElement_ . elementName

setElementId :: JSVal -> Text -> IO ()
setElementId element = setElementId_ element . toJSString . unpack

setElementClassName :: JSVal -> Text -> IO ()
setElementClassName element = setElementClassName_ element . toJSString . unpack

appendChildToBody :: JSVal -> IO ()
appendChildToBody = appendChildToBody_
