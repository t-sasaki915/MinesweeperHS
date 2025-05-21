module GameScreen (renderGameScreen) where

import           Miso

import           GameState (GameAction (..), GameState)
import           Textures  (Texture (..), Textures (..))

renderGameScreen :: GameState -> View GameAction
renderGameScreen _ =
    div_ [class_ "gameScreen"] $
        flip map [1..9 :: Int] $ \y ->
            div_ [class_ "gameRow"] $
                flip map [1..9 :: Int] $ \x ->
                    div_ [class_ "gameCell", onClick (CellClicked x y)]
                        [ textureSvg ClosedCellWithFlag
                        ]
