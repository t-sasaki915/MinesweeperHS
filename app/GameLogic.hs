module GameLogic (updateGameState) where

import           Control.Lens            (set, (^.))
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.State     (get, put)
import           Data.Functor            ((<&>))
import           Miso                    (Effect, io, io_)
import           Text.Printf             (printf)

import           GameLogic.MineGenerator (generateMines)
import           GameState

updateGameState :: GameAction -> Effect GameState GameAction
updateGameState (CellClicked clickedCell) = do
    state <- get

    case state ^. isGameStarted of
        True ->
            return ()

        False -> do
            put $ set isGameStarted True state

            io_ $ liftIO $ putStrLn (printf "Game started. First cell: %s" (show clickedCell))

            io $ liftIO $ generateMines clickedCell state <&> UpdateMines

updateGameState (UpdateMines generatedMines) =
    get >>= put . set cellsWithMine generatedMines
