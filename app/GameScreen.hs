module GameScreen (renderGameScreen) where

import           Miso
import           Miso.String          (ms)

import           GameState
import           GameState.Difficulty (Difficulties (..), Difficulty)

renderGameScreen :: Difficulty -> GameState -> View GameAction
renderGameScreen difficulty state = do
    let screenWidth = width $ difficulty
        screenHeight = height $ difficulty

    div_ [class_ "gameScreen"] $
        flip map [1..screenHeight] $ \y ->
            div_ [class_ "gameRow"] $
                flip map [1..screenWidth] $ \x -> do
                    let cell = Cell x y
                        cellClasses = ["gameCell"] ++
                            case hasCellMine cell state of
                                False -> ["closedCell"]
                                True  -> ["openedCellWithMine"]

                    div_ [class_ (ms $ unwords cellClasses), onClick (CellClicked cell)] []
