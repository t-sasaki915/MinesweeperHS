module Textures (Texture (..), Textures (..)) where

import           Miso        (View)
import           Miso.Mathml (xmlns_)
import           Miso.Svg

class Textures a where
    textureElements :: a -> [View x]

    textureSvg :: a -> View x
    textureSvg = svg_ [width_ "32", height_ "32", xmlns_ "http://www.w3.org/2000/svg"] . textureElements

data Texture = ClosedCell
              | OpenedCell
              | Flag
              | FlagPlaceholder
              | ClosedCellWithFlag
              | ClosedCellWithFlagPlaceholder
              | Mine
              | OpenedCellWithMine
              | HypocentreCell
              | Digit1
              | Digit2
              | Digit3
              | Digit4
              | Digit5
              | Digit6
              | Digit7
              | Digit8
              | OpenedCellWithDigit1
              | OpenedCellWithDigit2
              | OpenedCellWithDigit3
              | OpenedCellWithDigit4
              | OpenedCellWithDigit5
              | OpenedCellWithDigit6
              | OpenedCellWithDigit7
              | OpenedCellWithDigit8

instance Textures Texture where
    textureElements ClosedCell =
        [ rect_ [fill_ "#C0C0C0", width_ "32", height_ "32", x_ "0", y_ "0"] []
        , path_ [fill_ "#808080", d_ "M 32,0 26,6 26,32 32,32"] []
        , path_ [fill_ "#808080", d_ "M 0,32 6,26 32,26 32,32"] []
        , path_ [fill_ "#FFFFFF", d_ "M 0,0 32,0 26,6 0,6 0,0"] []
        , path_ [fill_ "#FFFFFF", d_ "M 0,32 6,26 6,0 0,0"] []
        ]

    textureElements OpenedCell =
        [ rect_ [fill_ "#C0C0C0", width_ "32", height_ "32", x_ "0", y_ "0"] []
        , rect_ [fill_ "#808080", width_ "32", height_ "1", x_ "0", y_ "0"] []
        , rect_ [fill_ "#808080", width_ "32", height_ "1", x_ "0", y_ "31"] []
        , rect_ [fill_ "#808080", width_ "1", height_ "32", x_ "0", y_ "0"] []
        , rect_ [fill_ "#808080", width_ "1", height_ "32", x_ "31", y_ "0"] []
        ]

    textureElements Flag =
        [ path_ [fill_ "#FF0000", d_ "M 18,7 18,17 9,12"] []
        , rect_ [fill_ "#000000", width_ "2", height_ "8", x_ "16", y_ "16"] []
        , path_ [fill_ "#000000", d_ "M 10,22 23,22 25,25 8,25"] []
        ]

    textureElements FlagPlaceholder =
        [ path_ [fill_ "#808080", d_ "M 18,7 18,17 9,12"] []
        , rect_ [fill_ "#808080", width_ "2", height_ "8", x_ "16", y_ "16"] []
        , path_ [fill_ "#808080", d_ "M 10,22 23,22 25,25 8,25"] []
        ]

    textureElements ClosedCellWithFlag =
        textureElements ClosedCell ++ textureElements Flag

    textureElements ClosedCellWithFlagPlaceholder =
        textureElements ClosedCell ++ textureElements FlagPlaceholder

    textureElements Mine =
        [ circle_ [fill_ "#000000", cx_ "16", cy_ "16", r_ "10"] []
        , rect_ [fill_ "#000000", width_ "2", height_ "28", x_ "15", y_ "2"] []
        , rect_ [fill_ "#000000", width_ "28", height_ "2", x_ "2", y_ "15"] []
        , rect_ [fill_ "#000000", width_ "26", height_ "2", x_ "2", y_ "13", transform_ "rotate(45, 13, 16)"] []
        , rect_ [fill_ "#000000", width_ "2", height_ "26", x_ "13", y_ "2", transform_ "rotate(45, 14, 17)"] []
        ]

    textureElements OpenedCellWithMine =
        textureElements OpenedCell ++ textureElements Mine

    textureElements HypocentreCell =
        [rect_ [fill_ "#FF0000", width_ "32", height_ "32", x_ "0", y_ "0"] []] ++ textureElements Mine

    textureElements Digit1 =
        [ rect_ [fill_ "#0000FF", width_ "4", height_ "24", x_ "14", y_ "4"] []
        , rect_ [fill_ "#0000FF", width_ "5", height_ "4", x_ "9", y_ "7"] []
        , rect_ [fill_ "#0000FF", width_ "16", height_ "4", x_ "8", y_ "24"] []
        ]

    textureElements Digit2 =
        [ rect_ [fill_ "#008000", width_ "4", height_ "14", x_ "8", y_ "14"] []
        , rect_ [fill_ "#008000", width_ "4", height_ "14", x_ "20", y_ "4"] []
        , rect_ [fill_ "#008000", width_ "16", height_ "4", x_ "8", y_ "4"] []
        , rect_ [fill_ "#008000", width_ "16", height_ "4", x_ "8", y_ "14"] []
        , rect_ [fill_ "#008000", width_ "16", height_ "4", x_ "8", y_ "24"] []
        ]

    textureElements Digit3 =
        [ rect_ [fill_ "#FF0000", width_ "4", height_ "14", x_ "20", y_ "4"] []
        , rect_ [fill_ "#FF0000", width_ "4", height_ "14", x_ "20", y_ "14"] []
        , rect_ [fill_ "#FF0000", width_ "16", height_ "4", x_ "8", y_ "4"] []
        , rect_ [fill_ "#FF0000", width_ "16", height_ "4", x_ "8", y_ "14"] []
        , rect_ [fill_ "#FF0000", width_ "16", height_ "4", x_ "8", y_ "24"] []
        ]

    textureElements Digit4 =
        [ rect_ [fill_ "#000080", width_ "4", height_ "14", x_ "8", y_ "4"] []
        , rect_ [fill_ "#000080", width_ "4", height_ "14", x_ "20", y_ "4"] []
        , rect_ [fill_ "#000080", width_ "4", height_ "14", x_ "20", y_ "14"] []
        , rect_ [fill_ "#000080", width_ "16", height_ "4", x_ "8", y_ "14"] []
        ]

    textureElements Digit5 =
        [ rect_ [fill_ "#800000", width_ "4", height_ "14", x_ "8", y_ "4"] []
        , rect_ [fill_ "#800000", width_ "4", height_ "14", x_ "20", y_ "14"] []
        , rect_ [fill_ "#800000", width_ "16", height_ "4", x_ "8", y_ "4"] []
        , rect_ [fill_ "#800000", width_ "16", height_ "4", x_ "8", y_ "14"] []
        , rect_ [fill_ "#800000", width_ "16", height_ "4", x_ "8", y_ "24"] []
        ]

    textureElements Digit6 =
        [ rect_ [fill_ "#008080", width_ "4", height_ "14", x_ "8", y_ "4"] []
        , rect_ [fill_ "#008080", width_ "4", height_ "14", x_ "8", y_ "14"] []
        , rect_ [fill_ "#008080", width_ "4", height_ "14", x_ "20", y_ "14"] []
        , rect_ [fill_ "#008080", width_ "16", height_ "4", x_ "8", y_ "4"] []
        , rect_ [fill_ "#008080", width_ "16", height_ "4", x_ "8", y_ "14"] []
        , rect_ [fill_ "#008080", width_ "16", height_ "4", x_ "8", y_ "24"] []
        ]

    textureElements Digit7 =
        [ rect_ [fill_ "#000000", width_ "4", height_ "14", x_ "20", y_ "4"] []
        , rect_ [fill_ "#000000", width_ "4", height_ "14", x_ "20", y_ "14"] []
        , rect_ [fill_ "#000000", width_ "16", height_ "4", x_ "8", y_ "4"] []
        ]

    textureElements Digit8 =
        [ rect_ [fill_ "#808080", width_ "4", height_ "14", x_ "8", y_ "4"] []
        , rect_ [fill_ "#808080", width_ "4", height_ "14", x_ "8", y_ "14"] []
        , rect_ [fill_ "#808080", width_ "4", height_ "14", x_ "20", y_ "4"] []
        , rect_ [fill_ "#808080", width_ "4", height_ "14", x_ "20", y_ "14"] []
        , rect_ [fill_ "#808080", width_ "16", height_ "4", x_ "8", y_ "4"] []
        , rect_ [fill_ "#808080", width_ "16", height_ "4", x_ "8", y_ "14"] []
        , rect_ [fill_ "#808080", width_ "16", height_ "4", x_ "8", y_ "24"] []
        ]

    textureElements OpenedCellWithDigit1 =
        textureElements OpenedCell ++ textureElements Digit1

    textureElements OpenedCellWithDigit2 =
        textureElements OpenedCell ++ textureElements Digit2

    textureElements OpenedCellWithDigit3 =
        textureElements OpenedCell ++ textureElements Digit3

    textureElements OpenedCellWithDigit4 =
        textureElements OpenedCell ++ textureElements Digit4

    textureElements OpenedCellWithDigit5 =
        textureElements OpenedCell ++ textureElements Digit5

    textureElements OpenedCellWithDigit6 =
        textureElements OpenedCell ++ textureElements Digit6

    textureElements OpenedCellWithDigit7 =
        textureElements OpenedCell ++ textureElements Digit7

    textureElements OpenedCellWithDigit8 =
        textureElements OpenedCell ++ textureElements Digit8
