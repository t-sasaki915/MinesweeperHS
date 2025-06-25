module GameLogic.Functions
    ( isGameRunning
    , canStartGame
    , isGameCleared
    , isGameInFlagPlacementMode
    , isGameInChordMode
    , enterFlagPlacementMode
    , exitFlagPlacementMode
    , enterChordMode
    , exitChordMode
    , isCellOpened
    , isCellFlagged
    , isCellMine
    , currentDifficulty
    , gameOver
    , revealMines
    , markWrongFlags
    , startGame
    , applyHypocentreTexture
    , applyOpenedCellTexture
    , applyNumberTextureToCell
    , applyFlagTextureToCell
    , applyFlagPlaceholderTextureToCell
    , removeFlagTextureFromCell
    , removeFlagPlaceholderTextureFromCell
    , appendToOpenedCells
    , appendToFlaggedCells
    , removeFromFlaggedCells
    , showFlagPlaceholders
    , hideFlagPlaceholders
    , updateFlagPlacementModeButtonText
    , updateChordModeButtonText
    , updateRemainingMinesLabel
    , aroundCells'
    ) where

import           Control.Lens                     (over, set, (^.))
import           Control.Monad                    (forM_)
import           Control.Monad.Extra              (orM, unlessM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT, get, put)
import           Data.List.Extra                  (cons)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Language.JavaScript.Wrapper

import           GameCell
import           GameDifficulty                   (GameDifficulty,
                                                   numberOfMines, screenHeight,
                                                   screenWidth)
import           GameState

isGameRunning :: Monad m => StateT GameState m Bool
isGameRunning = get >>= \state ->
    pure $ (state ^. isGameStarted) && not (state ^. isGameOver)

canStartGame :: Monad m => StateT GameState m Bool
canStartGame = get >>= \state ->
    pure $ not (state ^. isGameStarted) && not (state ^. isGameOver)

isGameCleared :: Monad m => StateT GameState m Bool
isGameCleared = get >>= \state ->
    currentDifficulty >>= \difficulty ->
        let width = screenWidth difficulty
            height = screenHeight difficulty
            numOfMines = numberOfMines difficulty in
                pure $ length (state ^. openedCells) >= ((width * height) - numOfMines)

isGameInFlagPlacementMode :: Monad m => StateT GameState m Bool
isGameInFlagPlacementMode = get >>= \state -> pure $ state ^. isFlagPlacementMode

isGameInChordMode :: Monad m => StateT GameState m Bool
isGameInChordMode = get >>= \state -> pure $ state ^. isChordMode

enterFlagPlacementMode :: Monad m => StateT GameState m ()
enterFlagPlacementMode = get >>= put . set isFlagPlacementMode True

exitFlagPlacementMode :: Monad m => StateT GameState m ()
exitFlagPlacementMode = get >>= put . set isFlagPlacementMode False

enterChordMode :: Monad m => StateT GameState m ()
enterChordMode = get >>= put . set isChordMode True

exitChordMode :: Monad m => StateT GameState m ()
exitChordMode = get >>= put . set isChordMode False

isCellOpened :: Monad m => GameCell -> StateT GameState m Bool
isCellOpened cell = get >>= \state -> pure $ cell `elem` (state ^. openedCells)

isCellFlagged :: Monad m => GameCell -> StateT GameState m Bool
isCellFlagged cell = get >>= \state -> pure $ cell `elem` (state ^. flaggedCells)

isCellMine :: Monad m => GameCell -> StateT GameState m Bool
isCellMine cell = get >>= \state -> pure $ cell `elem` (state ^. cellsWithMine)

currentDifficulty :: Monad m => StateT GameState m GameDifficulty
currentDifficulty = get >>= \state -> pure $ state ^. gameDifficulty

gameOver :: Monad m => StateT GameState m ()
gameOver = get >>= put . set isGameOver True

startGame :: Monad m => [GameCell] -> StateT GameState m ()
startGame generatedMines = get >>= put .
    set isGameStarted True .
        set cellsWithMine generatedMines

applyTextureToCell :: Text -> GameCell -> StateT GameState IO ()
applyTextureToCell texture cell = lift $
    getElementById (cellId cell) >>=
        setElementClassName texture

revealMines :: StateT GameState IO ()
revealMines = get >>= \state ->
    forM_ (state ^. cellsWithMine) $ \mineCell ->
        unlessM (isCellFlagged mineCell) $
            applyTextureToCell openedCellWithMineClass mineCell

markWrongFlags :: StateT GameState IO ()
markWrongFlags = get >>= \state ->
    forM_ (state ^. flaggedCells) $ \flaggedCell ->
        unlessM (isCellMine flaggedCell) $
            applyTextureToCell closedCellWithWrongFlagClass flaggedCell

applyHypocentreTexture :: GameCell -> StateT GameState IO ()
applyHypocentreTexture = applyTextureToCell hypocentreCellClass

applyOpenedCellTexture :: GameCell -> StateT GameState IO ()
applyOpenedCellTexture = applyTextureToCell openedCellClass

applyNumberTextureToCell :: Int -> GameCell -> StateT GameState IO ()
applyNumberTextureToCell n = applyTextureToCell (numberOnCellClass n)

applyFlagTextureToCell :: GameCell -> StateT GameState IO ()
applyFlagTextureToCell = applyTextureToCell closedCellWithFlagClass

applyFlagPlaceholderTextureToCell :: GameCell -> StateT GameState IO ()
applyFlagPlaceholderTextureToCell = applyTextureToCell closedCellWithFlagPlaceholderClass

removeFlagTextureFromCell :: GameCell -> StateT GameState IO ()
removeFlagTextureFromCell = applyTextureToCell closedCellClass

removeFlagPlaceholderTextureFromCell :: GameCell -> StateT GameState IO ()
removeFlagPlaceholderTextureFromCell = applyTextureToCell closedCellClass

appendToOpenedCells :: Monad m => GameCell -> StateT GameState m ()
appendToOpenedCells cell = get >>= put . over openedCells (cons cell)

appendToFlaggedCells :: Monad m => GameCell -> StateT GameState m ()
appendToFlaggedCells cell = get >>= put . over flaggedCells (cons cell)

removeFromFlaggedCells :: Monad m => GameCell -> StateT GameState m ()
removeFromFlaggedCells cell = get >>= put . over flaggedCells (filter (/= cell))

showFlagPlaceholders :: StateT GameState IO ()
showFlagPlaceholders = currentDifficulty >>= \difficulty ->
    forM_ (allCells difficulty) $ \cell ->
        unlessM (isCellFlagged cell `orM` isCellOpened cell) $
            applyFlagPlaceholderTextureToCell cell

hideFlagPlaceholders :: StateT GameState IO ()
hideFlagPlaceholders = currentDifficulty >>= \difficulty ->
    forM_ (allCells difficulty) $ \cell ->
        unlessM (isCellFlagged cell `orM` isCellOpened cell) $
            removeFlagPlaceholderTextureFromCell cell

updateFlagPlacementModeButtonText :: StateT GameState IO ()
updateFlagPlacementModeButtonText =
    isGameInFlagPlacementMode >>= \flagPlacementMode -> do
        buttonElem <- lift $ getElementById "flagPlacementModeButton"
        lift $ removeAllChildren buttonElem

        newTextNode <- lift $ createTextNode $
            if flagPlacementMode
                then "Exit Flag Placement Mode"
                else "Enter Flag Placement Mode"

        lift $ appendChild buttonElem newTextNode

updateChordModeButtonText :: StateT GameState IO ()
updateChordModeButtonText =
    isGameInChordMode >>= \chordMode -> do
        buttonElem <- lift $ getElementById "chordModeButton"
        lift $ removeAllChildren buttonElem

        newTextNode <- lift $ createTextNode $
            if chordMode
                then "Exit Chord Mode"
                else "Enter Chord Mode"

        lift $ appendChild buttonElem newTextNode

updateRemainingMinesLabel :: StateT GameState IO ()
updateRemainingMinesLabel = get >>= \state ->
    currentDifficulty >>= \difficulty -> do
        labelElem <- lift $ getElementById "remainingMinesLabel"
        lift $ removeAllChildren labelElem

        let remainingMines = numberOfMines difficulty - length (state ^. flaggedCells) in
            lift $ createTextNode (Text.show remainingMines) >>=
                appendChild labelElem

aroundCells' :: Monad m => GameCell -> StateT GameState m [GameCell]
aroundCells' centre = currentDifficulty >>= \difficulty ->
    pure $ aroundCells difficulty centre
