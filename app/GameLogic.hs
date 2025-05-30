module GameLogic
    ( onGameCellClicked
    , onGameCellRightClicked
    , onFlagPlacementModeButtonClicked
    , onChordModeButtonClicked
    , onRestartButtonClicked
    ) where

import           Control.Monad                    (filterM, forM_, when)
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
    isGameInFlagPlacementMode' <- isGameInFlagPlacementMode

    if isGameInFlagPlacementMode'
        then flagSequence clickedCell
        else do
            isGameInChordMode' <- isGameInChordMode

            if isGameInChordMode'
                then chordOpenSequence clickedCell
                else openSequence clickedCell

            whenM isGameRunning $
                whenM isGameCleared
                    clearSequence

onGameCellRightClicked :: GameCell -> StateT GameState IO ()
onGameCellRightClicked clickedCell =
    unlessM isGameInFlagPlacementMode $
        flagSequence clickedCell

onFlagPlacementModeButtonClicked :: StateT GameState IO ()
onFlagPlacementModeButtonClicked =
    whenM isGameRunning $ do
        isGameInFlagPlacementMode' <- isGameInFlagPlacementMode

        if isGameInFlagPlacementMode'
            then exitFlagPlacementMode >> hideFlagPlaceholders
            else enterFlagPlacementMode >> showFlagPlaceholders

        updateFlagPlacementModeButtonText

onChordModeButtonClicked :: StateT GameState IO ()
onChordModeButtonClicked =
    whenM isGameRunning $ do
        isGameInChordMode' <- isGameInChordMode

        if isGameInChordMode'
            then exitChordMode
            else enterChordMode

        updateChordModeButtonText

onRestartButtonClicked :: StateT GameState IO ()
onRestartButtonClicked = lift refreshPage

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

openSequence :: GameCell -> StateT GameState IO ()
openSequence cell = do
    whenM isGameRunning $
        unlessM (isCellOpened cell `orM` isCellFlagged cell) $ do
            openCell cell

    whenM canStartGame $ do
        generatedMines <- generateMines cell

        startGame generatedMines
        openCell cell

chordOpenSequence :: GameCell -> StateT GameState IO ()
chordOpenSequence cell =
    whenM isGameRunning $
        whenM (isCellOpened cell) $ do
            (SafeCell numberOnCell) <- calculateCellStatus cell

            around               <- aroundCells' cell
            numberOfFlaggedCells <- filterM isCellFlagged around <&> length

            when (numberOnCell == numberOfFlaggedCells) $
                forM_ around $ \c ->
                    unlessM (isCellOpened  c `orM` isCellFlagged c) $
                        openCell c

flagSequence :: GameCell -> StateT GameState IO ()
flagSequence cell =
    whenM isGameRunning $
        unlessM (isCellOpened cell) $ do
            isCellFlagged' <- isCellFlagged cell

            if isCellFlagged'
                then do
                    isGameInFlagPlacementMode' <- isGameInFlagPlacementMode

                    if isGameInFlagPlacementMode'
                        then do
                            applyFlagPlaceholderTextureToCell cell
                            removeFromFlaggedCells cell

                        else do
                            removeFlagTextureFromCell cell
                            removeFromFlaggedCells cell

                else do
                    applyFlagTextureToCell cell
                    appendToFlaggedCells cell

clearSequence :: StateT GameState IO ()
clearSequence = do
    lift $ alert "CLEAR"
    revealMines
    gameOver
