module Textures
    ( closedCellTexture
    , openedCellTexture
    ) where

import           Miso        (View)
import           Miso.Mathml (xmlns_)
import           Miso.Svg

closedCellTexture :: View a
closedCellTexture =
    svg_ [width_ "32", height_ "32", xmlns_"http://www.w3.org/2000/svg"]
        [ rect_ [width_ "32", height_ "32", x_ "0", y_ "0", fill_ "#C0C0C0"] []
        , path_ [fill_ "#808080", d_ "M 32,0 26,6 26,32 32,32"] []
        , path_ [fill_ "#808080", d_ "M 0,32 6,26 32,26 32,32"] []
        , path_ [fill_ "#FFFFFF", d_ "M 0,0 32,0 26,6 0,6 0,0"] []
        , path_ [fill_ "#FFFFFF", d_ "M 0,32 6,26 6,0 0,0"] []
        ]

openedCellTexture :: View a
openedCellTexture =
    svg_ [width_ "32", height_ "32", xmlns_ "http://www.w3.org/2000/svg"]
        [ rect_ [width_ "32", height_ "32", x_ "0", y_ "0", fill_ "#C0C0C0"] []
        , rect_ [width_ "32", height_ "1", x_ "0", y_ "0", fill_ "#808080"] []
        , rect_ [width_ "32", height_ "1", x_ "0", y_ "31", fill_ "#808080"] []
        , rect_ [width_ "1", height_ "32", x_ "0", y_ "0", fill_ "#808080"] []
        , rect_ [width_ "1", height_ "32", x_ "31", y_ "0", fill_ "#808080"] []
        ]
