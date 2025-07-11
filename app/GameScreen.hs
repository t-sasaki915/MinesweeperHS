module GameScreen
    ( renderGameScreen
    , initialiseGameButtons
    , renderDifficultySelector
    , initialiseGameStatusLabels
    ) where

import           Control.Monad               (forM_, when)
import           Data.String.Here            (i)
import qualified Data.Text                   as Text
import           Language.JavaScript.Wrapper

import           GameCell                    (GameCell (..), cellId,
                                              closedCellClass)
import           GameDifficulty
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
            addEventListener MiddleClick (onGameCellMiddleClicked gameCell) cellElem

            appendChild rowElem cellElem

        appendChild gameContainer rowElem

initialiseGameButtons :: IO ()
initialiseGameButtons = do
    flagPlacementModeButton <- getElementById "flagPlacementModeButton"
    addEventListener Click onFlagPlacementModeButtonClicked flagPlacementModeButton

    chordModeButton <- getElementById "chordModeButton"
    addEventListener Click onChordModeButtonClicked chordModeButton

    restartButton <- getElementById "restartButton"
    addEventListener Click onRestartButtonClicked restartButton

renderDifficultySelector :: GameDifficulty -> IO ()
renderDifficultySelector currentDifficulty = do
    difficultySelector <- getElementById "difficultySelector"

    forM_ allDifficulties $ \difficulty -> do
        optionElem <- createElement Option
        if difficulty /= defaultGameDifficulty
            then setElementValue [i|/?difficulty=${difficulty}|] optionElem
            else setElementValue "/" optionElem
        when (currentDifficulty == difficulty) $
            setIsElementSelected True optionElem

        createTextNode (Text.show difficulty) >>=
            appendChild optionElem

        appendChild difficultySelector optionElem

initialiseGameStatusLabels :: GameDifficulty -> IO ()
initialiseGameStatusLabels difficulty = do
    remainingMinesLabelElem <- getElementById "remainingMinesLabel"

    removeAllChildren remainingMinesLabelElem
    createTextNode (Text.show $ numberOfMines difficulty) >>=
        appendChild remainingMinesLabelElem
