module Main (main) where

import           Language.JavaScript.Framework (initialiseAppState)
import           Language.JavaScript.Wrapper   (getURLSearchParam)

import           Data.Maybe                    (fromMaybe)
import           GameDifficulty                (defaultGameDifficulty,
                                                gameDifficultyFromText)
import           GameScreen
import           GameState                     (initialGameState)

main :: IO ()
main = do
    difficulty <-
        getURLSearchParam "difficulty" >>= \case
            Nothing -> return defaultGameDifficulty
            Just txt ->
                return $ fromMaybe defaultGameDifficulty (gameDifficultyFromText txt)

    initialiseAppState (initialGameState difficulty)

    renderGameScreen difficulty
    renderGameButtons
    renderDifficultySelector difficulty
