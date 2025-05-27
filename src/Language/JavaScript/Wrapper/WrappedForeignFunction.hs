module Language.JavaScript.Wrapper.WrappedForeignFunction
    ( consoleLog
    , createElement
    , setElementId
    , setElementClassName
    , appendChildToBody
    ) where

import           Data.Functor                                 ((<&>))
import           Data.Text                                    (Text, unpack)
import           GHC.JS.Prim                                  (toJSString)
import           Language.JavaScript.Wrapper.Element          (Element (..),
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

appendChildToBody :: Element -> IO ()
appendChildToBody (Element element) = appendChildToBody_ element
