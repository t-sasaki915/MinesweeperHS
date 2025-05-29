module GameLogic.Functions
    ( isGameRunning
    , canStartGame
    , isGameCleared
    , isCellOpened
    , isCellFlagged
    , isCellMine
    , currentDifficulty
    , gameOver
    , revealMines
    , markWrongFlags
    , startGame
    , setHypocentre
    , applyOpenedCellClass
    , applyNumberOnCell
    , applyFlagToCell
    , removeFlagFromCell
    , appendToOpenedCells
    , appendToFlaggedCells
    , removeFromFlaggedCells
    ) where

import           Control.Lens                     (over, set, (^.))
import           Control.Monad                    (forM_)
import           Control.Monad.Extra              (unlessM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT, get, put)
import           Data.List.Extra                  (cons)
import           Data.Text                        (Text)
import           Language.JavaScript.Wrapper

import           GameCell
import           GameDifficulty
import           GameState

isGameRunning :: Monad m => StateT GameState m Bool
isGameRunning = get >>= \state ->
    return $ (state ^. isGameStarted) && not (state ^. isGameOver)

canStartGame :: Monad m => StateT GameState m Bool
canStartGame = get >>= \state ->
    return $ not (state ^. isGameStarted) && not (state ^. isGameOver)

isGameCleared :: Monad m => StateT GameState m Bool
isGameCleared = get >>= \state ->
    currentDifficulty >>= \difficulty ->
        let width = screenWidth difficulty
            height = screenHeight difficulty
            numOfMines = numberOfMines difficulty in
                return $ length (state ^. openedCells) >= ((width * height) - numOfMines)

isCellOpened :: Monad m => GameCell -> StateT GameState m Bool
isCellOpened cell = get >>= \state -> return $ cell `elem` (state ^. openedCells)

isCellFlagged :: Monad m => GameCell -> StateT GameState m Bool
isCellFlagged cell = get >>= \state -> return $ cell `elem` (state ^. flaggedCells)

isCellMine :: Monad m => GameCell -> StateT GameState m Bool
isCellMine cell = get >>= \state -> return $ cell `elem` (state ^. cellsWithMine)

currentDifficulty :: Monad m => StateT GameState m GameDifficulty
currentDifficulty = get >>= \state -> return $ state ^. gameDifficulty

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

setHypocentre :: GameCell -> StateT GameState IO ()
setHypocentre = applyTextureToCell hypocentreCellClass

applyOpenedCellClass :: GameCell -> StateT GameState IO ()
applyOpenedCellClass = applyTextureToCell openedCellClass

applyNumberOnCell :: Int -> GameCell -> StateT GameState IO ()
applyNumberOnCell n = applyTextureToCell (numberOnCellClass n)

applyFlagToCell :: GameCell -> StateT GameState IO ()
applyFlagToCell = applyTextureToCell closedCellWithFlagClass

removeFlagFromCell :: GameCell -> StateT GameState IO ()
removeFlagFromCell = applyTextureToCell closedCellClass

appendToOpenedCells :: Monad m => GameCell -> StateT GameState m ()
appendToOpenedCells cell = get >>= put . over openedCells (cons cell)

appendToFlaggedCells :: Monad m => GameCell -> StateT GameState m ()
appendToFlaggedCells cell = get >>= put . over flaggedCells (cons cell)

removeFromFlaggedCells :: Monad m => GameCell -> StateT GameState m ()
removeFromFlaggedCells cell = get >>= put . over flaggedCells (filter (/= cell))
