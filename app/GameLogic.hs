module GameLogic
    ( onGameCellClicked
    , onGameCellRightClicked
    , onRestartButtonClicked
    ) where

import           Control.Monad                    (filterM, forM_)
import           Control.Monad.Extra              (orM, unlessM, whenM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT)
import           Data.Functor                     ((<&>))
import           Language.JavaScript.Wrapper

import           GameCell                         (GameCell)
import           GameLogic.Functions
import           GameLogic.MineGenerator          (generateMines)
import           GameState                        (GameState)

onGameCellClicked :: GameCell -> StateT GameState IO ()
onGameCellClicked clickedCell = do
    whenM isGameRunning $
        unlessM (isCellOpened clickedCell `orM` isCellFlagged clickedCell) $ do
            openCell clickedCell

    whenM canStartGame $ do
        generatedMines <- generateMines clickedCell

        startGame generatedMines
        openCell clickedCell

    whenM isGameRunning $
        whenM isGameCleared $
            clearSequence

onGameCellRightClicked :: GameCell -> StateT GameState IO ()
onGameCellRightClicked clickedCell =
    whenM isGameRunning $
        unlessM (isCellOpened clickedCell) $ do
            isCellFlagged' <- isCellFlagged clickedCell

            if isCellFlagged'
                then removeFlagTextureFromCell clickedCell >> removeFromFlaggedCells clickedCell
                else applyFlagTextureToCell clickedCell >> appendToFlaggedCells clickedCell


onRestartButtonClicked :: StateT GameState IO ()
onRestartButtonClicked = lift $ refreshPage


openCell :: GameCell -> StateT GameState IO ()
openCell cell =
    calculateCellStatus cell >>= \case
        MineCell -> do
            revealMines
            markWrongFlags
            applyHypocentreTexture cell
            gameOver

        (SafeCell 0) -> do
            applyOpenedCellTexture cell
            appendToOpenedCells cell

            around <- aroundCells' cell
            forM_ around $ \c ->
                unlessM (isCellOpened c `orM` isCellFlagged c) $
                    openCell c

        (SafeCell numberOnCell) -> do
            applyNumberTextureToCell numberOnCell cell
            appendToOpenedCells cell


data GameCellStatus = MineCell
                    | SafeCell Int
                    deriving Eq

calculateCellStatus :: Monad m => GameCell -> StateT GameState m GameCellStatus
calculateCellStatus cell = do
    around <- aroundCells' cell

    isCellMine' <- isCellMine cell
    if isCellMine'
        then return MineCell
        else filterM isCellMine around <&> SafeCell . length


clearSequence :: StateT GameState IO ()
clearSequence = do
    lift $ alert "CLEAR"
    revealMines
    gameOver
