module Internal.MoreAttributes
    exposing (..)


import Element                                  exposing (..)
import Html
import Html.Attributes          as HtmlAttr

import Css
import Html.Styled
import Html.Styled.Attributes                   exposing ( css
                                                         )

style = styleFromString

styleFromString : String -> String -> Attribute msg
styleFromString name value =
    HtmlAttr.style name value |> htmlAttribute


noOutline : Attribute msg
noOutline = styleFromString "box-shadow" "none"


id : String -> Attribute msg
id id_
    = HtmlAttr.id id_ |> htmlAttribute


title : String -> Attribute msg
title title_
    = HtmlAttr.title title_ |> htmlAttribute

fixedTop : Attribute msg
fixedTop
    = HtmlAttr.style "position" "fixed" |> htmlAttribute


paddingTop : Int -> Attribute msg
paddingTop top
    = Element.paddingEach { top = top
                          , left = 0
                          , right = 0
                          , bottom  = 0
                          }

paddingBottom : Int -> Attribute msg
paddingBottom bottom
    = Element.paddingEach { top = 0
                          , left = 0
                          , right = 0
                          , bottom  = bottom
                          }

positionAbsolute
    = styleFromString "position" "absolute"


passPointerEvents
    = styleFromString "pointer-events" "none"


scrollbarWidthNone
    = styleFromString "scrollbar-width" "none"


msOverFlowStyleNone
    = styleFromString "-ms-overflow-style" "none"


cssNonStandardWebkitScrollbarDisplayNone
    = css [ Css.pseudoElement "-webkit-scrollbar" [ Css.display Css.none ]
          , Css.width (Css.pct 100)
          , Css.border (Css.px 5)
          , Css.borderColor (Css.rgb 0 0 0)
          , Css.overflowX Css.hidden
          , Css.overflowY Css.scroll
          , Css.property "scrollbar-width" "none"
          ]
