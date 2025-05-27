module GameScreen (renderGameScreen) where

import           Control.Monad               (forM_)
import           Data.Text                   (pack)
import           Language.JavaScript.Wrapper
import           Text.Printf                 (printf)

import           GameCell                    (GameCell (..))
import           GameDifficulty              (GameDifficulty, screenHeight,
                                              screenWidth)
import           GameLogic                   (onGameCellClicked)

renderGameScreen :: GameDifficulty -> IO ()
renderGameScreen difficulty = do
    gameContainer <- getElementById "gameContainer"

    forM_ [1..(screenHeight difficulty)] $ \y -> do
        rowElem <- createElement Div
        setElementClassName "gameRow" rowElem

        forM_ [1..(screenWidth difficulty)] $ \x -> do
            let gameCell = GameCell x y

            cellElem <- createElement Div
            setElementId (pack $ printf "gameCell_%d_%d" x y) cellElem
            setElementClassName "gameCell closedCell" cellElem
            addEventListenerWithState Click (onGameCellClicked gameCell) cellElem

            appendChild rowElem cellElem

        appendChild gameContainer rowElem
