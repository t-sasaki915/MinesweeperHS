module Language.JavaScript.Wrapper.WrappedForeignFunction
    ( createElement
    , setElementId
    , setElementClassName
    , appendChild
    , getElementById
    , addEventListener
    ) where

import           Data.Functor                                 ((<&>))
import           Data.Text                                    (Text, unpack)
import           GHC.JS.Foreign.Callback                      (asyncCallback)
import           GHC.JS.Prim                                  (toJSString)
import           Language.JavaScript.Wrapper.ElementWrapper   (Element (..),
                                                               ElementType,
                                                               elementTypeName)
import           Language.JavaScript.Wrapper.EventWrapper     (EventType (..),
                                                               eventTypeName)
import           Language.JavaScript.Wrapper.Internal.Foreign

createElement :: ElementType -> IO Element
createElement elementType = createElement_ (elementTypeName elementType) <&> Element

setElementId :: Text -> Element -> IO ()
setElementId newId (Element element) = setElementId_ (toJSString $ unpack newId) element

setElementClassName :: Text -> Element -> IO ()
setElementClassName newClassName (Element element) =
    setElementClassName_ (toJSString $ unpack newClassName) element

appendChild :: Element -> Element -> IO ()
appendChild (Element parent) (Element child) = appendChild_ parent child

getElementById :: Text -> IO Element
getElementById elementId = getElementById_ (toJSString $ unpack elementId) <&> Element

addEventListener :: EventType -> IO () -> Element -> IO ()
addEventListener eventType listener (Element element) =
    asyncCallback listener >>= \callback ->
        addEventListener_ (eventTypeName eventType) callback element
