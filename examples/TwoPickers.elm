module TwoPickers
        exposing ( .. )

{-| This first example shows how to use BaseScrollPicker
-}

import Dict                                     exposing (Dict)
import Task

import Html                                     exposing (Html)
import Element                                  exposing ( .. )
import Element.Font             as Font
import Element.Background       as Background

import Elmnt.BaseScrollPicker   as ScrollPicker

import Browser

sizeScaled
    = Element.modular 16 1.25 >> round

defaultFontSize : Int
defaultFontSize
    = sizeScaled 8


-- -- -- SIMPLE EXAMPLE -- -- --

{-| This module has internal state so all the message
required to wrap (or map) to the your own Msg type.

**Note:** Int type in (Msg *Int* ExampleMsg) is the type for options.
Please Checkout [`Option`](#option) for further information.
-}
type ExampleMsg
    = ScrollPickerMessage String (ScrollPicker.Msg Int ExampleMsg)


{-| There are two separate picker in this example, first picker will choose
hour value, second one will choose minute value. and to deal with proper model
with update, messageMapWith will take a id(String) as first argument.

**Note:** pickerDirection means scrolling [`Direction`](#direction). And the name of
messageMapWith, pickerDirection is used in the [`scrollPicker`](#scrollPicker)
so it might be handy if you keep the same name.
-}
type alias ExampleModel
    = { firstPickerState  : ScrollPicker.MinimalState Int ExampleMsg
      , secondPickerState : ScrollPicker.MinimalState Int ExampleMsg
      , messageMapWith    : String -> (ScrollPicker.Msg Int ExampleMsg) -> ExampleMsg
      , pickerDirection   : ScrollPicker.Direction
      , hourValue         : Int
      , minuteValue       : Int
      }


{-| Initialise our example model. Each picker model can be initialised with
[`initMinimalState`](#initMinimalState) and [`setOptions`](#setOptions)

And you might need to set default value for the picker.
how the example does
-}
exampleInit : () -> ( ExampleModel, Cmd ExampleMsg )
exampleInit flags
    = ( { firstPickerState -- for hour value
              = ScrollPicker.initMinimalState "firstScrollPicker"
                |> ScrollPicker.setOptions
                   (String.fromInt)
                   (List.range 1 12
                      |> List.map
                         ( \n -> ( n
                                 , n |> ( String.fromInt >> text )
                                 )
                         )
                   )
                |> ScrollPicker.setScrollStopCheckTime 75

        , secondPickerState -- for minute value
              = ScrollPicker.initMinimalState "secondScrollPicker"
                |> ScrollPicker.setOptions
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
        , pickerDirection = ScrollPicker.Vertical
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
                         ScrollPicker.MoveToTargetOption <|
                             ScrollPicker.getOptionIdString
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
              = ScrollPicker.updateWith model
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
                                  
                          in ( case ScrollPicker.anyNewOptionSelected
                                    pickerMsg of
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
                                  
                          in ( case ScrollPicker.anyNewOptionSelected
                                    pickerMsg of
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
            = ScrollPicker.defaultTheme

        pickerHelper
            = ScrollPicker.viewAsElement model theme

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

               , el [ paddingEach
                      { top    = 20
                      , right  = 0
                      , bottom = 0
                      , left   = 0
                      }
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
          |> ScrollPicker.subscriptionsWith
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
