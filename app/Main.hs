module Main (main) where

import           Language.JavaScript.Wrapper

main :: IO ()
main = do
    element <- createElement Div
    setElementId "asdf" element
    setElementClassName "gameCell closedCell" element

    appendChildToBody element

    consoleLog "TEST"
