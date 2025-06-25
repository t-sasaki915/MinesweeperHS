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
            Nothing -> pure defaultGameDifficulty
            Just txt ->
                pure $ fromMaybe defaultGameDifficulty (gameDifficultyFromText txt)

    initialiseAppState (initialGameState difficulty)

    renderGameScreen difficulty
    initialiseGameButtons
    renderDifficultySelector difficulty
    initialiseGameStatusLabels difficulty
