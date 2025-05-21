module Textures (Texture (..), Textures (..)) where

import           Miso        (View)
import           Miso.Mathml (xmlns_)
import           Miso.Svg

class Texture a where
    textureElements :: a -> [View x]
    
    textureSvg :: a -> View x
    textureSvg = svg_ [width_ "32", height_ "32", xmlns_ "http://www.w3.org/2000/svg"] . textureElements

data Textures = ClosedCell
              | OpenedCell
              | Flag
              | ClosedCellWithFlag
              | Mine
              | OpenedCellWithMine
              | HypocentreCell

instance Texture Textures where
    textureElements ClosedCell =
        [ rect_ [width_ "32", height_ "32", x_ "0", y_ "0", fill_ "#C0C0C0"] []
        , path_ [fill_ "#808080", d_ "M 32,0 26,6 26,32 32,32"] []
        , path_ [fill_ "#808080", d_ "M 0,32 6,26 32,26 32,32"] []
        , path_ [fill_ "#FFFFFF", d_ "M 0,0 32,0 26,6 0,6 0,0"] []
        , path_ [fill_ "#FFFFFF", d_ "M 0,32 6,26 6,0 0,0"] []
        ]
    
    textureElements OpenedCell =
        [ rect_ [width_ "32", height_ "32", x_ "0", y_ "0", fill_ "#C0C0C0"] []
        , rect_ [width_ "32", height_ "1", x_ "0", y_ "0", fill_ "#808080"] []
        , rect_ [width_ "32", height_ "1", x_ "0", y_ "31", fill_ "#808080"] []
        , rect_ [width_ "1", height_ "32", x_ "0", y_ "0", fill_ "#808080"] []
        , rect_ [width_ "1", height_ "32", x_ "31", y_ "0", fill_ "#808080"] []
        ]
    
    textureElements Flag =
        [ path_ [fill_ "#FF0000", d_ "M 18,7 18,17 9,12"] []
        , rect_ [width_ "2", height_ "8", x_ "16", y_ "16", fill_ "#000000"] []
        , path_ [fill_ "#000000", d_ "M 10,22 23,22 25,25 8,25"] []
        ]
    
    textureElements ClosedCellWithFlag =
        textureElements ClosedCell ++ textureElements Flag

    textureElements Mine =
        [ circle_ [cx_ "16", cy_ "16", r_ "10", fill_ "#000000"] []
        , rect_ [width_ "2", height_ "28", x_ "15", y_ "2", fill_ "#000000"] []
        , rect_ [width_ "28", height_ "2", x_ "2", y_ "15", fill_ "#000000"] []
        , rect_ [width_ "26", height_ "2", x_ "2", y_ "13", transform_ "rotate(45, 13, 16)", fill_ "#000000"] []
        , rect_ [width_ "2", height_ "26", x_ "13", y_ "2", transform_ "rotate(45, 14, 17)", fill_ "#000000"] []
        ]
    
    textureElements OpenedCellWithMine =
        textureElements OpenedCell ++ textureElements Mine

    textureElements HypocentreCell =
        [rect_ [width_ "32", height_ "32", x_ "0", y_ "0", fill_ "#FF0000"] []] ++ textureElements Mine
