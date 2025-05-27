module GameLogic (onGameCellClicked) where

import           Control.Lens                     (set, (^.))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT, get, put)
import           Data.Text                        (pack)
import           Language.JavaScript.Wrapper      (consoleLog)
import           Text.Printf                      (printf)

import           GameCell                         (GameCell (..))
import           GameState

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
