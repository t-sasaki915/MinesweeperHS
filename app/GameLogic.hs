module GameLogic
    ( onGameCellClicked
    , onGameCellRightClicked
    ) where

import           Control.Lens                     (over, set, (^.))
import           Control.Monad                    (forM_, unless, when)
import           Control.Monad.Extra              (whenM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT, get, put)
import           Data.List.Extra                  (cons)
import           Data.Maybe                       (fromJust)
import           Language.JavaScript.Wrapper

import           GameCell
import           GameDifficulty
import           GameLogic.MineGenerator          (generateMines)
import           GameState

onGameCellClicked :: GameCell -> StateT GameState IO ()
onGameCellClicked clickedCell = do
    state <- get

    let gameStarted = state ^. isGameStarted
        gameOver    = state ^. isGameOver

    when (gameStarted && not gameOver) $
        whenM (isCellClosed clickedCell) $ do
            openCell clickedCell

            checkIfCleared

    when (not gameStarted && not gameOver) $ do
        generatedMines <- lift $ generateMines (state ^. gameDifficulty) clickedCell

        put $
            set isGameStarted True $
                set cellsWithMine generatedMines state

        openCell clickedCell

        checkIfCleared


onGameCellRightClicked :: GameCell -> StateT GameState IO ()
onGameCellRightClicked clickedCell = do
    state <- get

    whenM (isCellClosed clickedCell) $
        if clickedCell `notElem` (state ^. flaggedCells) then do
            cellElem <- lift $ getElementById (cellId clickedCell)
            lift $ setElementClassName closedCellWithFlagClass cellElem

            put $ over flaggedCells (cons clickedCell) state

        else do
            cellElem <- lift $ getElementById (cellId clickedCell)
            lift $ setElementClassName closedCellClass cellElem

            put $ over flaggedCells (filter (/= clickedCell)) state

checkIfCleared :: StateT GameState IO ()
checkIfCleared = do
    state <- get

    let difficulty = state ^. gameDifficulty
        width = screenWidth difficulty
        height = screenHeight difficulty
        numOfMines = numberOfMines difficulty

    when (length (state ^. openedCells) >= ((width * height) - numOfMines)) $ do
        lift $ alert "Clear"

        revealMines

        put $ set isGameOver True state

openCell :: GameCell -> StateT GameState IO ()
openCell cell = do
    state <- get

    calculateCellStatus cell >>= \case
        IsMine -> do
            revealMines

            hypocentreElem <- lift $ getElementById (cellId cell)
            lift $ setElementClassName hypocentreCellClass hypocentreElem

            put $ set isGameOver True state

        Zero -> do
            cellElem <- lift $ getElementById (cellId cell)
            lift $ setElementClassName openedCellClass cellElem

            put $ over openedCells (cons cell) state

            let around = aroundCells (state ^. gameDifficulty) cell
            forM_ around $ \c -> do
                newState <- get

                unless (c `elem` newState ^. openedCells) $
                    openCell c

        numberOnCell -> do
            cellElem <- lift $ getElementById (cellId cell)
            lift $ setElementClassName (fromJust $ numberOnCellClass numberOnCell) cellElem

            put $ over openedCells (cons cell) state

isCellClosed :: Monad m => GameCell -> StateT GameState m Bool
isCellClosed cell = get >>= \state -> return $ cell `notElem` state ^. openedCells

revealMines :: StateT GameState IO ()
revealMines = get >>= \state ->
    forM_ (state ^. cellsWithMine) $ \mineCell -> do
        mineCellElem <- lift $ getElementById (cellId mineCell)
        lift $ setElementClassName openedCellWithMineClass mineCellElem

calculateCellStatus :: Monad m => GameCell -> StateT GameState m GameCellStatus
calculateCellStatus cell = do
    state <- get

    let mines = state ^. cellsWithMine
        around = aroundCells (state ^. gameDifficulty) cell

    if cell `notElem` mines
        then return $
                case length $ filter (`elem` mines) around of
                    1 -> One
                    2 -> Two
                    3 -> Three
                    4 -> Four
                    5 -> Five
                    6 -> Six
                    7 -> Seven
                    8 -> Eight
                    0 -> Zero
                    _ -> error "impossible"
        else return IsMine
