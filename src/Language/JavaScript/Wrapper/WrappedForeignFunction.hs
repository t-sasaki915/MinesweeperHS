module Language.JavaScript.Wrapper.WrappedForeignFunction
    ( consoleLog
    , createElement
    , setElementId
    , setElementClassName
    , appendChild
    , getElementById
    ) where

import           Data.Functor                                 ((<&>))
import           Data.Text                                    (Text, unpack)
import           GHC.JS.Prim                                  (toJSString)
import           Language.JavaScript.Wrapper.ElementWrapper   (Element (..),
                                                               ElementType,
                                                               elementTypeName)
import           Language.JavaScript.Wrapper.Internal.Foreign

consoleLog :: Text -> IO ()
consoleLog = consoleLog_ . toJSString . unpack

createElement :: ElementType -> IO Element
createElement elementType = createElement_ (elementTypeName elementType) <&> Element

setElementId :: Text -> Element -> IO ()
setElementId newId (Element element) = setElementId_ (toJSString $ unpack newId) element

setElementClassName :: Text -> Element -> IO ()
setElementClassName newClassName (Element element) = setElementClassName_ (toJSString $ unpack newClassName) element

appendChild :: Element -> Element -> IO ()
appendChild (Element parent) (Element child) = appendChild_ parent child

getElementById :: Text -> IO Element
getElementById elementId = getElementById_ (toJSString $ unpack elementId) <&> Element
