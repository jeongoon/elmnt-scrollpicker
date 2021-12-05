module Elmnt.Theme
    exposing ( .. )


import Color                                    exposing ( Color )
import Internal.Palette         as Palette
import Internal.ColorUtil       as ColorUtil


type alias Palette
    = Palette.Palette

type Value a
    = Default
    | Custom a


isDefault : Value a -> Bool
isDefault val
    = case val of
          Default ->
              True
          _ ->
              False
    
withDefault : a -> Value a -> a
withDefault default unknownValue
    = case unknownValue of
          Default ->
              default
 
          Custom value ->
              value


defaultPalette
    = lightPalette


lightPalette : Palette
lightPalette
    = let
        lowContrast -- lowContrast to `light'
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
