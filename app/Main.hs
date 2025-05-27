module Main (main) where

import           Control.Monad               (forM_)
import qualified Data.Text                   as Text
import           Language.JavaScript.Wrapper
import           Text.Printf                 (printf)

main :: IO ()
main = do
    gameContainer <- getElementById "gameContainer"

    forM_ [1..9] $ \y -> do
        rowElem <- createElement Div
        setElementClassName "gameRow" rowElem

        forM_ [1..9] $ \x -> do
            cellElem <- createElement Div

            setElementId (Text.pack $ printf "gameCell_%d_%d" x y) cellElem
            setElementClassName "gameCell closedCell" cellElem

            appendChild rowElem cellElem

        appendChild gameContainer rowElem
