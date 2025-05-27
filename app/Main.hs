{-# LANGUAGE CPP #-}

module Main (main) where

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
    appendChildToDocument :: JSVal -> IO ()

main :: IO ()
main = do
    element <- createElement (toJSString "div")
    setElementId element (toJSString "asdf")
    setElementClasses element (toJSString "gameCell closedCell")

    appendChildToDocument element

    consoleLog (toJSString "TEST")
