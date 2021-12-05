module Internal.Palette
    exposing ( Palette
             , PaletteWith )


import Color                                    exposing ( Color )
import Element


type alias BasePaletteWith colorType
    = { primary         : colorType
      , secondary       : colorType
      , success         : colorType
      , info            : colorType
      , warning         : colorType
      , danger          : colorType
      , accent          : colorType
      , surface         : colorType
      , background      : colorType
      }


type alias PaletteWith colorType
    = { primary         : colorType
      , secondary       : colorType
      , success         : colorType
      , info            : colorType
      , warning         : colorType
      , danger          : colorType
      , accent          : colorType
      , surface         : colorType
      , background      : colorType
      , on              : BasePaletteWith colorType
      , toElmUiColor    : colorType -> Element.Color
      -- ^ when writing or draw on the specific color
      --   we need some distingushable color 
      }


-- Default color type is 'Color'
type alias Palette
    = PaletteWith Color
