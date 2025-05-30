module GameLogic.MineGenerator (generateMines) where

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT)
import           Language.JavaScript.Wrapper      (randomInt)

import           GameCell                         (GameCell (..))
import           GameDifficulty                   (GameDifficulty,
                                                   numberOfMines, screenHeight,
                                                   screenWidth)
import           GameLogic.Functions              (aroundCells',
                                                   currentDifficulty)
import           GameState                        (GameState)

generateMines :: GameCell -> StateT GameState IO [GameCell]
generateMines firstCell = do
    difficulty <- currentDifficulty
    safeSpaces <- aroundCells' firstCell

    lift $ generateMines' difficulty safeSpaces []
    where
        generateMines' :: GameDifficulty -> [GameCell] -> [GameCell] -> IO [GameCell]
        generateMines' difficulty safeSpaces generatedMines
            | length generatedMines == numberOfMines difficulty =
                return generatedMines

            | otherwise = do
                randomX <- randomInt 1 (screenWidth difficulty)
                randomY <- randomInt 1 (screenHeight difficulty)

                let mineCandidate = GameCell randomX randomY
                    adoptedMines =
                        [mineCandidate |
                            mineCandidate `notElem` safeSpaces
                                && mineCandidate `notElem` generatedMines]

                generateMines' difficulty safeSpaces (generatedMines ++ adoptedMines)
