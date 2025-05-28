module GameLogic.MineGenerator (generateMines) where

import           GameCell                    (GameCell (..), aroundCells)
import           GameDifficulty              (GameDifficulty, numberOfMines,
                                              screenHeight, screenWidth)
import           Language.JavaScript.Wrapper (randomInt)

generateMines :: GameDifficulty -> GameCell -> IO [GameCell]
generateMines difficulty firstCell = do
    let safeSpaces = aroundCells difficulty firstCell

    generateMines' safeSpaces []
    where
        generateMines' :: [GameCell] -> [GameCell] -> IO [GameCell]
        generateMines' safeSpaces generatedMines
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

                generateMines' safeSpaces (generatedMines ++ adoptedMines)
