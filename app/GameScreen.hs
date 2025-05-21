module GameScreen (renderGameScreen) where

import           Control.Lens         ((^.))
import           Miso

import           GameState
import           GameState.Difficulty (Difficulties (..))
import           Textures             (Texture (..), Textures (..))

renderGameScreen :: GameState -> View GameAction
renderGameScreen state = do
    let screenWidth = width $ state ^. difficulty
        screenHeight = height $ state ^. difficulty

    div_ [class_ "gameScreen"] $
        flip map [1..screenHeight] $ \y ->
            div_ [class_ "gameRow"] $
                flip map [1..screenWidth] $ \x -> do
                    let cell = Cell x y

                    div_ [class_ "gameCell", onClick (CellClicked cell)] $
                        return $ textureSvg $
                            case hasCellMine cell state of
                                False -> OpenedCell
                                True  -> OpenedCellWithMine
