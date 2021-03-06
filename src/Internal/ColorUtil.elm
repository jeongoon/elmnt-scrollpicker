module Internal.ColorUtil
    exposing (..)

import Color                                    exposing ( Color )

import Html
import Html.Attributes  as HtmlAttr

import Css
import Element


replaceAlpha : Float -> Color -> Color
replaceAlpha alpha color
    = let
        rgba = Color.toRgba color
   in
       Color.fromRgba { rgba |
                        alpha = alpha
                      }


rgbaStringFromColor : Color -> String
rgbaStringFromColor color
    = let
        rgba
            = Color.toRgba color

      in
          "rgba(" ++
              (List.map
                   (\f ->
                        rgba |> (f >> ((*)255) >> truncate >> String.fromInt))
                   [ .red
                   , .green
                   , .blue
                   ]
              |> String.join ","
              ) ++
              "," ++ (String.fromFloat rgba.alpha) ++ ")"

    
{-| toStyleRgba - set Html attribute with style name and Color data type
-}
toStyleRgba : String -> Color -> Html.Attribute msg
toStyleRgba name color
    =  rgbaStringFromColor color
    |> HtmlAttr.style name 


{-| toStyledColor : this functions converts Element.Color -> Css.Color
-}
toStyledColor : Color -> Css.Color
toStyledColor color
    = let
        rgba
            = Color.toRgba color

        get fn
            = (fn >> truncate) rgba

   in Css.rgba (get .red) (get .green) (get .blue) (rgba.alpha)


{-| toElmUIColor
-}
toElmUiColor : Color -> Element.Color
toElmUiColor = (Color.toRgba >> Element.fromRgb)
