module Main (main) where

import           Language.JavaScript.Wrapper

main :: IO ()
main = do
    gameContainer <- getElementById "gameContainer"

    element <- createElement Div
    setElementId "asdf" element
    setElementClassName "gameCell closedCell" element

    appendChild gameContainer element

    consoleLog "TEST"
