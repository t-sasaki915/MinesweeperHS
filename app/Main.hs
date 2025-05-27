module Main (main) where

import           Language.JavaScript.Framework (initialiseAppState)

import           GameDifficulty                (GameDifficulty (..))
import           GameScreen                    (renderGameScreen)
import           GameState                     (initialGameState)

main :: IO ()
main = do
    let difficulty = Hard

    initialiseAppState (initialGameState difficulty)

    renderGameScreen difficulty
