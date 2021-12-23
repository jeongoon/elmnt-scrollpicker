module Elmnt.Theme
    exposing ( isDefault
             , withDefault
             , Value (..)
             , Palette
             , defaultPalette
             , lightPalette
             , PaletteType
             , PaletteOnType
             )

{-| This module provides default / custom value data type and shows an example of
'Palette' type with 'Color'

# Default / Custom value

@docs Value, isDefault, withDefault

# Palette

@docs Palette, defaultPalette, lightPalette

-}

import Element
import Color                                    exposing ( Color )
import Internal.Palette         as Palette
import Internal.ColorUtil       as ColorUtil


{-| Recommened Palette type is PaletteWith [`Color`](packages/avh4/elm-color/latest)
-}
type alias Palette
    = Palette.Palette

type alias PaletteType
    = { primary : Color
      , secondary : Color
      , success : Color
      , info : Color
      , warning : Color
      , danger : Color
      , accent : Color
      , surface : Color
      , background : Color
      , on : PaletteOnType
      , toElmUiColor
          : (Color -> Element.Color)
      }

type alias PaletteOnType
    = { primary : Color
      , secondary : Color
      , success : Color
      , info : Color
      , warning : Color
      , danger : Color
      , accent : Color
      , surface : Color
      , background : Color
      }

{-| Each theme value can be default or custom value.

The function which uses 'Default' value should know how to evaluate or set the default value.
-}
type Value a
    = Default
    | Custom a


{-| Helper function to determine the given value is an Default value
-}
isDefault : Value a -> Bool
isDefault val
    = case val of
          Default ->
              True
          _ ->
              False
    
{-| It works similar to Maybe.withDefault

Helps to set a value to some variable or field with default value if
user does not supply any custom value.

```elm
        fontSize
            = Theme.withDefault
                  defaultFontSize theme.fontSize
```

or

```elm
        fontSize
            = theme.fontSize
            |> Theme.withDefault defaultFontSize
```

depends on your preference.

-}
withDefault : a -> Value a -> a
withDefault default unknownValue
    = case unknownValue of
          Default ->
              default
 
          Custom value ->
              value


{-| default palette for [`defaultTheme`][defaultTheme] and
[`viewElement`][viewAsElement]

[defaultTheme]: /packages/jeongoon/elmnt-scrollpicker/latest/Elmnt-BaseScrollPicker#defaultTheme
[viewAsElement]: /packages/jeongoon/elmnt-scrollpicker/latest/Elmnt-BaseScrollPicker#viewAsElement


-}
defaultPalette : Palette
defaultPalette
    = lightPalette

{-| Just an example of light palette

```elm
lightPalette : Palette
lightPalette
    = let
        lowContrast
         -- ^ lowContrast to `light' : dark colour
            = Color.rgb255 0xE7 0xE0 0xD0
        highContrast
            = Color.rgb255 0x14 0x11 0x11

   in
       { primary
             = Color.rgb255 0x3D 0x5A 0x75
       , secondary
             = Color.rgb255 0x82 0x8C 0x74
       , success
             = Color.rgb255 0x74 0xAC 0x88
       , info
             = Color.rgb255 0x7F 0xAE 0xE2
       , warning
             = Color.rgb255 0xFC 0xC3 0x5C
       , danger
             = Color.rgb255 0xB5 0x05 0x46
       , accent
             = Color.rgb255 0x05 0x45 0x76
       , surface
             = lowContrast
       , background
             = Color.rgb255 0xEC 0xFB 0xED

      , on = { primary
                   = lowContrast
             , secondary
                   = highContrast
             , success
                   = lowContrast
             , info
                   = highContrast
             , warning
                   = lowContrast
             , danger
                   = lowContrast
             , accent
                   = lowContrast
             , surface
                   = highContrast
             , background
                   = lowContrast
             }
       , toElmUiColor
           = ColorUtil.toElmUiColor
      }
```
-}
lightPalette : Palette
lightPalette
    = let
        lowContrast -- lowContrast to `light' : dark colour
            = Color.rgb255 0xE7 0xE0 0xD0
        highContrast
            = Color.rgb255 0x14 0x11 0x11

   in
       { primary
             = Color.rgb255 0x3D 0x5A 0x75
       , secondary
             = Color.rgb255 0x82 0x8C 0x74
       , success
             = Color.rgb255 0x74 0xAC 0x88
       , info
             = Color.rgb255 0x7F 0xAE 0xE2
       , warning
             = Color.rgb255 0xFC 0xC3 0x5C
       , danger
             = Color.rgb255 0xB5 0x05 0x46
       , accent
             = Color.rgb255 0x05 0x45 0x76
       , surface
             = lowContrast
       , background
             = Color.rgb255 0xEC 0xFB 0xED

      , on = { primary
                   = lowContrast
             , secondary
                   = highContrast
             , success
                   = lowContrast
             , info
                   = highContrast
             , warning
                   = lowContrast
             , danger
                   = lowContrast
             , accent
                   = lowContrast
             , surface
                   = highContrast
             , background
                   = lowContrast
             }
       , toElmUiColor
           = ColorUtil.toElmUiColor
      }
