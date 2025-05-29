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

revealMines :: StateT GameState IO ()
revealMines = get >>= \state ->
    forM_ (state ^. cellsWithMine) $ \mineCell ->
        unlessM (isCellFlagged mineCell) $ lift $
            getElementById (cellId mineCell) >>=
                setElementClassName openedCellWithMineClass

startGame :: Monad m => [GameCell] -> StateT GameState m ()
startGame generatedMines = get >>= put .
    set isGameStarted True .
        set cellsWithMine generatedMines

setHypocentre :: GameCell -> StateT GameState IO ()
setHypocentre hypocentre = lift $
    getElementById (cellId hypocentre) >>=
        setElementClassName hypocentreCellClass

applyOpenedCellClass :: GameCell -> StateT GameState IO ()
applyOpenedCellClass cell = lift $
    getElementById (cellId cell) >>=
        setElementClassName openedCellClass

applyNumberOnCell :: Int -> GameCell -> StateT GameState IO ()
applyNumberOnCell n cell = lift $
    getElementById (cellId cell) >>=
        setElementClassName (numberOnCellClass n)

applyFlagToCell :: GameCell -> StateT GameState IO ()
applyFlagToCell cell = lift $
    getElementById (cellId cell) >>=
        setElementClassName closedCellWithFlagClass

removeFlagFromCell :: GameCell -> StateT GameState IO ()
removeFlagFromCell cell = lift $
    getElementById (cellId cell) >>=
        setElementClassName closedCellClass

appendToOpenedCells :: Monad m => GameCell -> StateT GameState m ()
appendToOpenedCells cell = get >>= put . over openedCells (cons cell)

appendToFlaggedCells :: Monad m => GameCell -> StateT GameState m ()
appendToFlaggedCells cell = get >>= put . over flaggedCells (cons cell)

removeFromFlaggedCells :: Monad m => GameCell -> StateT GameState m ()
removeFromFlaggedCells cell = get >>= put . over flaggedCells (filter (/= cell))
