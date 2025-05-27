module Main (main) where

import           Control.Lens                     (set, (^.))
import           Control.Monad                    (forM_)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT, get, put)
import           Data.Text                        (pack)
import           Language.JavaScript.Framework
import           Language.JavaScript.Wrapper
import           Text.Printf                      (printf)

import           GameCell
import           GameState

main :: IO ()
main = do
    initialiseAppState initialGameState

    gameContainer <- getElementById "gameContainer"

    forM_ [1..9] $ \y -> do
        rowElem <- createElement Div
        setElementClassName "gameRow" rowElem

        forM_ [1..9] $ \x -> do
            let gameCell = GameCell x y

            cellElem <- createElement Div

            setElementId (pack $ printf "gameCell_%d_%d" x y) cellElem
            setElementClassName "gameCell closedCell" cellElem
            addEventListenerWithState Click (onGameCellClicked gameCell) cellElem

            appendChild rowElem cellElem

        appendChild gameContainer rowElem

onGameCellClicked :: GameCell -> StateT GameState IO ()
onGameCellClicked clickedCell = do
    state <- get

    if state ^. isGameStarted then
        lift $ consoleLog (pack $ printf "Cell clicked: %s" (show clickedCell))

    else do
        lift $ consoleLog (pack $ printf "Game started. First cell: %s" (show clickedCell))

        put $
            set isGameStarted True $
                set firstGameCell (Just clickedCell) state
