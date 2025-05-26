module GameLogic (updateGameState) where

import           Control.Monad.IO.Class  (liftIO)
import           Data.Functor            ((<&>))
import           Miso                    (Effect, get, io, io_, put)
import           Text.Printf             (printf)

import           GameLogic.MineGenerator (generateMines)
import           GameState
import           GameState.Difficulty    (Difficulty)

updateGameState :: Difficulty -> GameAction -> Effect GameState GameAction
updateGameState difficulty (CellClicked clickedCell) = do
    get >>= \case
        NotStarted -> do
            io_ $ liftIO $ putStrLn (printf "Game started. First cell: %s" (show clickedCell))

            io $ liftIO $ generateMines clickedCell difficulty <&> UpdateMines

        state@GameStarted {} -> do
            io_ $ liftIO $ putStrLn (printf "Cell clicked: %s" (show clickedCell))

updateGameState _ (UpdateMines generatedMines) =
    put $ GameStarted
        { cellsWithMine = generatedMines
        }
