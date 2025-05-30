module GameScreen
    ( renderGameScreen
    , renderGameButtons
    , renderDifficultySelector
    ) where

import           Control.Monad               (forM_, when)
import           Data.Text                   (pack)
import qualified Data.Text                   as Text
import           Language.JavaScript.Wrapper
import           Text.Printf                 (printf)

import           GameCell                    (GameCell (..), cellId,
                                              closedCellClass)
import           GameDifficulty              (GameDifficulty, allDifficulties,
                                              defaultGameDifficulty,
                                              screenHeight, screenWidth)
import           GameLogic

renderGameScreen :: GameDifficulty -> IO ()
renderGameScreen difficulty = do
    gameContainer <- getElementById "gameContainer"

    forM_ [1..(screenHeight difficulty)] $ \y -> do
        rowElem <- createElement Div
        setElementClassName "gameRow" rowElem

        forM_ [1..(screenWidth difficulty)] $ \x -> do
            let gameCell = GameCell x y

            cellElem <- createElement Div
            setElementId (cellId gameCell) cellElem
            setElementClassName closedCellClass cellElem
            addEventListener Click (onGameCellClicked gameCell) cellElem
            addEventListener RightClick (onGameCellRightClicked gameCell) cellElem

            appendChild rowElem cellElem

        appendChild gameContainer rowElem

renderGameButtons :: IO ()
renderGameButtons = do
    flagPlacementModeButton <- getElementById "flagPlacementModeButton"
    addEventListener Click onFlagPlacementModeButtonClicked flagPlacementModeButton

    restartButton <- getElementById "restartButton"
    addEventListener Click onRestartButtonClicked restartButton

renderDifficultySelector :: GameDifficulty -> IO ()
renderDifficultySelector currentDifficulty = do
    difficultySelector <- getElementById "difficultySelector"

    forM_ allDifficulties $ \difficulty -> do
        optionElem <- createElement Option
        if difficulty /= defaultGameDifficulty
            then setElementValue (pack $ printf "/?difficulty=%s" (show difficulty)) optionElem
            else setElementValue "/" optionElem
        when (currentDifficulty == difficulty) $
            setIsElementSelected True optionElem

        createTextNode (Text.show difficulty) >>=
            appendChild optionElem

        appendChild difficultySelector optionElem
