module Language.JavaScript.Wrapper.ElementWrapper
    ( Element (..)
    , ElementType (..)
    , createElement
    , createTextNode
    , setElementId
    , setElementClassName
    , setElementInnerHtml
    , setElementValue
    , setIsElementSelected
    , appendChild
    , getElementById
    , removeAllChildren
    ) where

import           Data.Functor                                 ((<&>))
import           Data.Text                                    (Text, unpack)
import           GHC.JS.Prim                                  (JSVal,
                                                               toJSString)
import           Language.JavaScript.Wrapper.Internal.Foreign

newtype Element = Element JSVal

data ElementType = Div
                 | Option
                 deriving (Show, Eq)

elementTypeName :: ElementType -> JSVal
elementTypeName Div    = toJSString "div"
elementTypeName Option = toJSString "option"

createElement :: ElementType -> IO Element
createElement elementType = createElement_ (elementTypeName elementType) <&> Element

createTextNode :: Text -> IO Element
createTextNode text = createTextNode_ (toJSString $ unpack text) <&> Element

setElementId :: Text -> Element -> IO ()
setElementId newId (Element element) = setElementId_ (toJSString $ unpack newId) element

setElementClassName :: Text -> Element -> IO ()
setElementClassName newClassName (Element element) =
    setElementClassName_ (toJSString $ unpack newClassName) element

setElementInnerHtml :: Element -> Element -> IO ()
setElementInnerHtml (Element newInner) (Element element) =
    setElementInnerHtml_ newInner element

setElementValue :: Text -> Element -> IO ()
setElementValue newValue (Element element) = setElementValue_ (toJSString $ unpack newValue) element

setIsElementSelected :: Bool -> Element -> IO ()
setIsElementSelected newIsSelected (Element element) =
    setIsElementSelected_ (toJSString $ if newIsSelected then "true" else "false") element

appendChild :: Element -> Element -> IO ()
appendChild (Element parent) (Element child) = appendChild_ parent child

getElementById :: Text -> IO Element
getElementById elementId = getElementById_ (toJSString $ unpack elementId) <&> Element

removeAllChildren :: Element -> IO ()
removeAllChildren (Element element) = removeAllChildren_ element
