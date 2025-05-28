module GameLogic (onGameCellClicked) where

import           Control.Lens                     (set, (^.))
import           Control.Monad                    (forM_)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT, get, put)
import           Data.Text                        (pack)
import           Language.JavaScript.Wrapper
import           Text.Printf                      (printf)

import           GameCell                         (GameCell (..), cellId)
import           GameLogic.MineGenerator          (generateMines)
import           GameState

onGameCellClicked :: GameCell -> StateT GameState IO ()
onGameCellClicked clickedCell = do
    state <- get

    if state ^. isGameStarted then
        lift $ consoleLog (pack $ printf "Cell clicked: %s" (show clickedCell))

    else do
        lift $ consoleLog (pack $ printf "Game started. First cell: %s" (show clickedCell))

        generatedMines <- lift $ generateMines (state ^. gameDifficulty) clickedCell

        put $
            set isGameStarted True $
                set cellsWithMine generatedMines state

        forM_ generatedMines $ \mineCell -> do
            cellElem <- lift $ getElementById (cellId mineCell)
            lift $ setElementClassName "gameCell openedCellWithMine" cellElem
