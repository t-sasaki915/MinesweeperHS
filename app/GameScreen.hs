module GameScreen (renderGameScreen) where

import           Miso
import           Miso.String          (ms)
import           Miso.Svg             (g_, svg_, transform_)

import           GameState
import           GameState.Difficulty (Difficulties (..), Difficulty)
import           Text.Printf          (printf)
import           Textures             (Texture (..), Textures (..))

renderGameScreen :: Difficulty -> GameState -> View GameAction
renderGameScreen difficulty state = do
    let screenWidth = width $ difficulty
        screenHeight = height $ difficulty

    svg_ [width_ (ms $ 32 * screenWidth), height_ (ms $ 32 * screenHeight)] $
        flip map [(x, y) | x <- [1..screenWidth], y <- [1..screenHeight]] $ \(x, y) -> do
            let cell = Cell x y

            g_ [transform_ (ms (printf "translate(%d,%d)" (32 * (x - 1)) (32 * (y - 1)) :: String)), onClick (CellClicked cell)] $
                textureElements $
                    case hasCellMine cell state of
                        False -> OpenedCell
                        True  -> OpenedCellWithMine
