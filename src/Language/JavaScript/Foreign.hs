module Language.JavaScript.Foreign
    ( consoleLog
    , createElement
    , setElementId
    , setElementClasses
    , appendChildToBody
    ) where

import           GHC.JS.Prim

foreign import javascript "((message) => console.log(message))"
    consoleLog :: JSVal -> IO ()

foreign import javascript "((elementName) => document.createElement(elementName))"
    createElement :: JSVal -> IO JSVal

foreign import javascript "((element, newId) => element.id = newId)"
    setElementId :: JSVal -> JSVal -> IO ()

foreign import javascript "((element, newClasses) => element.className = newClasses)"
    setElementClasses :: JSVal -> JSVal -> IO ()

foreign import javascript "((element) => document.body.appendChild(element))"
    appendChildToBody :: JSVal -> IO ()
