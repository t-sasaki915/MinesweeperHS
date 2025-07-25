module Language.JavaScript.Wrapper.Internal.Foreign
    ( consoleLog_
    , alert_
    , refreshPage_
    , createElement_
    , createTextNode_
    , setElementId_
    , setElementClassName_
    , setElementInnerHtml_
    , setElementValue_
    , setIsElementSelected_
    , appendChild_
    , getElementById_
    , addLeftClickEventListener_
    , addRightClickEventListener_
    , addMiddleClickEventListener_
    , removeAllChildren_
    , getURLSearchParam_
    , randomInt_
    ) where

import           GHC.JS.Foreign.Callback (Callback)
import           GHC.JS.Prim             (JSVal)

foreign import javascript "((message) => console.log(message))"
    consoleLog_ :: JSVal -> IO ()

foreign import javascript "((message) => alert(message))"
    alert_ :: JSVal -> IO ()

foreign import javascript "(() => location.reload())"
    refreshPage_ :: IO ()

foreign import javascript "((elementTypeName) => document.createElement(elementTypeName))"
    createElement_ :: JSVal -> IO JSVal

foreign import javascript "((text) => document.createTextNode(text))"
    createTextNode_ :: JSVal -> IO JSVal

foreign import javascript "((newId, element) => element.id = newId)"
    setElementId_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((newClassName, element) => element.className = newClassName)"
    setElementClassName_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((newInner, element) => element.innerHTML = newInner)"
    setElementInnerHtml_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((newValue, element) => element.value = newValue)"
    setElementValue_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((newIsSelected, element) => element.selected = newIsSelected)"
    setIsElementSelected_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((parent, child) => parent.appendChild(child))"
    appendChild_ :: JSVal -> JSVal -> IO ()

foreign import javascript "((elementId) => document.getElementById(elementId))"
    getElementById_ :: JSVal -> IO JSVal

foreign import javascript "((listener, element) => element.addEventListener('click', listener))"
    addLeftClickEventListener_ :: Callback (IO ()) -> JSVal -> IO ()

foreign import javascript "((listener, element) => element.addEventListener('contextmenu', (e) => { e.preventDefault(); listener(); }))"
    addRightClickEventListener_ :: Callback (IO ()) -> JSVal -> IO ()

foreign import javascript "((listener, element) => element.addEventListener('auxclick', (e) => { if (e.which == 2) { e.preventDefault(); listener(); } }))"
    addMiddleClickEventListener_ :: Callback (IO ()) -> JSVal -> IO ()

foreign import javascript "((element) => element.textContent = '')"
    removeAllChildren_ :: JSVal -> IO ()

foreign import javascript "((paramName) => new URLSearchParams(document.location.search).get(paramName))"
    getURLSearchParam_ :: JSVal -> IO JSVal

foreign import javascript "((min, max) => Math.floor(Math.random() * (max - min + 1)) + min)"
    randomInt_ :: Int -> Int -> IO Int
