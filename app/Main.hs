module Main (main) where

import           Language.JavaScript.Wrapper

main :: IO ()
main = do
    element <- createElement Div
    setElementId element "asdf"
    setElementClassName element "gameCell closedCell"

    appendChildToBody element

    consoleLog "TEST"
