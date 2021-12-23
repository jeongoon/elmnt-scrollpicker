module Elmnt.ScrollPicker
        exposing ( .. )

                    
{-| This module is an extened scroll picker from BaseScrollPicker
-}

import Dict                                     exposing (Dict)
import Set                                      exposing (Set)
import Time
import Process
import Task                                     exposing ( Task )

import Element                                  exposing (..)
-- ^ this module is basically based on the elm-ui

import Element.Font             as Font
import Element.Input            as Input
import Element.Border           as Border
import Element.Background       as Background
import Element.Region           as Region

import Color                                    exposing ( Color )
-- ^ this is better choice for color `period'

import Html                                     exposing ( Html )

import Json.Decode              as Decode

import Css
import Html.Styled
import Html.Styled.Attributes                   exposing ( css )
import Html.Styled.Events

import Browser
import Browser.Dom
import Browser.Events

import Animation
import Animation.Messenger

-- -- -- Internal modules and helpers -- -- --
import Elmnt.BaseScrollPicker   as Base         exposing ( .. )
import Elmnt.Theme              as Theme
import Internal.Util            as Util
import Internal.MoreAttributes  as MAttr
import Internal.ColorUtil       as ColorUtil
import Internal.Palette         as Palette      exposing ( Palette
                                                         , PaletteWith
                                                         )
{-| If you want to change your shape of options depends on the position
this option type is for you.
-}
type alias Option vt msg
    = { idString           : String
      , index              : Int
      , value              : vt
      , element            : Element msg
      , longitudinalLength : Float
      }


{-- TODO 
make updateWith function 

update position always and when need it, just use it.
but I need to change the BaseScrollPicker as well due to
which function should I use for onSuccess ...
or use default onSuccess and change it in this module to
work with condition: option attributes change depends on the location

--}

initState idString
    = { idString
            = idString
      , optionIds
            = []
      , optionIdToRecordDict
            = Dict.empty
      , targetIdString
            = Nothing
      , pseudoAnimState
            = initPseudoAnimState 0
      , lastScrollClock
            = Time.millisToPosix 0
      , scrollTraceMP
            = Set.empty
      , finalTargetScrollPosMP
            = -1
      , scrollStopCheckTime
            = 250 -- 250 ms
      , mbOptionIdToExtraDict
            = Nothing
      }

-- -- -- SIMPLE EXAMPLE -- -- --

defaultFontSize : Int
defaultFontSize
    = Util.sizeScaled 8

{-| This module has internal state so all the message
required to wrap (or map) to the your own Msg type.

**Note:** Int type in (Msg *Int* ExampleMsg) is the type for options.
Please Checkout [`Option`](#option) for further information.
-}
type ExampleMsg
    = ScrollPickerMessage String (Msg Int ExampleMsg)


{-| -}
type alias ExampleModel
    = { firstPickerState  : State Int ExampleMsg
      , secondPickerState : State Int ExampleMsg
      , messageMapWith    : String -> (Msg Int ExampleMsg) -> ExampleMsg
      , pickerDirection   : Direction
      , hourValue         : Int
      , minuteValue       : Int
      }


{-| -}
exampleInit : () -> ( ExampleModel, Cmd ExampleMsg )
exampleInit flags
    = ( { firstPickerState -- for hour value
              = initState "firstScrollPicker"
                |> setOptions
                   (String.fromInt)
                   (List.range 1 12
                      |> List.map
                         ( \n -> ( n
                                 , n |> ( String.fromInt >> text )
                                 )
                         )
                   )
                |> setScrollStopCheckTime 75

        , secondPickerState -- for minute value
              = initState "secondScrollPicker"
                |> setOptions
                   (String.fromInt)
                   (List.range 0 59
                      |> List.map
                         ( \n -> ( n
                                 , n |> ( String.fromInt
                                              >> String.padLeft 2 '0'
                                              >> text
                                        )
                                 )
                         )
                   )

        , messageMapWith = ScrollPickerMessage
          -- ^ a map function to wrap the picker messages into the ExampleMsg
        , pickerDirection = Vertical
        , hourValue = 5
        , minuteValue = 32
        }              

      -- v focus to initial values
      , [ ( "firstScrollPicker", 5 )
        , ( "secondScrollPicker" , 32) ]
        |> List.map
           ( \(pickerIdString, optionValue) ->
                 Task.perform identity <|
                     Task.succeed <| ScrollPickerMessage pickerIdString <|
                         MoveToTargetOption <| getOptionIdString
                                               String.fromInt
                                               pickerIdString optionValue
           )
      |> Cmd.batch
      )


{-| and inside your update function you can check picker Id and update approriate
picker model.

**Suggestion**: You can put your picker model into Dict or List depends on your
preference.
-}
exampleUpdate : ExampleMsg -> ExampleModel -> ( ExampleModel, Cmd ExampleMsg )
exampleUpdate msg model
    = let update
              = updateWith model
      in
          case msg of
              ScrollPickerMessage idString pickerMsg ->
                  case idString of
                      "firstScrollPicker" ->
                          let ( firstPickerState, cmd )
                                  = update pickerMsg model.firstPickerState

                              newModel
                                  = { model |
                                      firstPickerState
                                          = firstPickerState
                                    }
                                  
                          in ( case anyNewOptionSelected pickerMsg of
                                   Just option ->
                                       { newModel |
                                         hourValue = option.value
                                       }
                                   Nothing ->
                                       newModel
                               , cmd
                             )

                      "secondScrollPicker" ->
                          let ( secondPickerState, cmd )
                                  = update pickerMsg model.secondPickerState

                              newModel
                                  = { model |
                                      secondPickerState
                                          = secondPickerState
                                    }
                                  
                          in ( case anyNewOptionSelected pickerMsg of
                                   Just option ->
                                       { newModel |
                                         minuteValue = option.value
                                       }
                                   Nothing ->
                                       newModel
                               , cmd
                             )

                      _ ->
                          ( model, Cmd.none )

{-| exampleView shows how to reveal the model on the page by using elm-ui
check out which settings you can change [`defaultTheme`](#defaultTheme)
-}
exampleView : ExampleModel -> Html ExampleMsg
exampleView model
    = let
        theme
            = defaultTheme

        pickerHelper
            = viewAsElement model theme

   in
       layout [ Background.color (theme.palette.on.surface -- use same color as shade
                                      |> theme.palette.toElmUiColor)
              ] <|
           column [ centerX
                  , centerY
                  ]
               [ row [ spacing 1
                     ]
                     [ pickerHelper model.firstPickerState
                     , pickerHelper model.secondPickerState
                     ]

               , el [ MAttr.paddingTop 20
                    , Font.size
                        ( defaultFontSize
                              |> toFloat
                              |> (*) 0.7
                              |> truncate
                        )
                    , Font.color
                          ( theme.palette.secondary
                                |> theme.palette.toElmUiColor )
                    , centerX
                    ] <|
                   text <| "It's " ++
                       ( model.hourValue
                             |> String.fromInt
                       ) ++ ":" ++
                       ( model.minuteValue
                             |> String.fromInt
                             |> String.padLeft 2 '0'
                       )
           ]
 


{-| Scroll picker relies on animation by using elm-style-animation
so subscriptions is essential to work with this module this is acheived easily.
-}
exampleSubscriptions : ExampleModel -> Sub ExampleMsg
exampleSubscriptions model
    = Sub.batch
      [ model
          |> subscriptionsWith
             [ model.firstPickerState
             , model.secondPickerState
             ]
      ]


{-| Finally you can make main function with all the functions associated.
-}
main : Program () ExampleModel ExampleMsg
main
    = Browser.element
      { init = exampleInit
      , view = exampleView
      , update = exampleUpdate
      , subscriptions = exampleSubscriptions
      }

----
defaultOptionAttrsAndLengthWith : (BaseTheme (MinimalPalette palette
                                                  (MinimalPaletteOn paletteOn) msg
                                             )
                                  ) ->
                                  OptionAttrsFunction

defaultOptionAttrsAndLengthWith pickerDirection pickerLength framePos optionPos 
                                option optionAbsPos theme
    = let getCenterPos oneDim
              = 0.5 * ( oneDim.endPoint - oneDim.startPoint )
              
          ( attrs, actualLength )
              = if optionPos.endPoint > framePos.startPoint &&
                        optionPos.startPoint < framePos.endPoint then
                    let distance
                            =

                        enlargeFactor
                            = ( distance / pickerLength )
                            |> abs
                            |> cos
                            |> ( (*) 1000
                                 >> truncate
                                 >> (flip (//) 1000) )
                           
                        option.longitudinalLength * 
                    in
                        


                    getOptionAttrsAndLength
                        getCenterPos optionPos
                        - getCenterPos framePos

                else
                    ( [], options.longitudinalLength )

      in

