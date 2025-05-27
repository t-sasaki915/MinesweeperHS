module Main (main) where

import           GHC.JS.Prim                 (toJSString)
import           Language.JavaScript.Foreign

main :: IO ()
main = do
    element <- createElement (toJSString "div")
    setElementId element (toJSString "asdf")
    setElementClasses element (toJSString "gameCell closedCell")

    appendChildToBody element

    consoleLog (toJSString "TEST")
