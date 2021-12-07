[setOptions]: /packages/jeongoon/elmnt-scrollpicker/latest/Elmnt-BaseSCrollPicker#setOptions
[initMinimalModel]: /packages/jeongoon/elmnt-scrollpicker/latest/Elmnt-BaseSCrollPicker#initMinimalModel
[defaultTheme]: /packages/jeongoon/elmnt-scrollpicker/latest/Elmnt-BaseSCrollPicker#defaultTheme
[MinimalModel]: /packages/jeongoon/elmnt-scrollpicker/latest/Elmnt-BaseSCrollPicker#MinimalModel
[exampleUrl]: https://jeongoon.github.io/examples/7Dec2021.BaseScrollPicker.html

# An Elm-Ui friendly Scroll Picker

`elmnt-scrollpicker` provides an scroll picker with some animation. `elmnt`
is stands for [`Element`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
so you can use the View(widget) as an element in elm-ui.

[See it in action here.][exampleUrl]

# How to use

## Import

```elm
import Element exposing (..)
import Dict exposing (Dict)
import Elmnt.BaseScrollPicker as ScrollPicker
```

## Make your own Model and Msg

```elm

    type ExampleMsg
        = ScrollPickerMessage String (ScrollPicker.Msg Int ExampleMsg)
```

Unfortuneately, some states of picker are required to store in
internal record. you might need to declare your own message wrapper
constructor *ScrollPickerMessage* is an wrapper constructor (or map function) to
create messages which is compatible to your own module message.


Let's say we are making a simple time picker, we need two seprate
pickers for hour and minute value.


```elm
type alias ExampleModel -- which is your own model
    = { firstPickerModel  : ScrollPicker.MinimalModel Int ExampleMsg
      , secondPickerModel : ScrollPicker.MinimalModel Int ExampleMsg
      , messageMapWith    : String -> (ScrollPicker.Msg Int ExampleMsg) -> ExampleMsg
      , pickerDirection   : ScrollPicker.Direction -- Horizontal or Vertical
      }
```


## View
exampleView shows how to reveal the model on the page by using elm-ui.

check out which settings you can change [`defaultTheme`][defaultTheme]

```elm
exampleView : ExampleModel -> Html ExampleMsg
exampleView model
    = let
        theme
            = ScrollPicker.defaultTheme

        picker
            = scrollPicker.viewAsElement model theme

   in
       layout [ Background.color <|
                theme.palette.toElmUiColor theme.palette.surface
              ] <|
           row [ spacing 1
               , centerX
               , centerY
               ]
           [ picker model.firstPickerModel
           , picker model.secondPickerModel
           ]
```

## Model

[`MinimalModel`][MinimalModel]
you can still use any other API in the module to work with your own *picker*
model As most of API use partial record type. for example [`setOptions`][setOptions]
function has the definition like below

```elm
setOptions : (vt -> String) ->
             List (vt, Element msg) ->
             { state |
               idString  : String
             , optionIds : List String
             , optionIdToRecordDict : Dict String (Option vt msg)
             } -> -- At least, those fields are required in the state record
             { state |
               idString  : String
             , optionIds : List String
             , optionIdToRecordDict : Dict String (Option vt msg)
             } --> will return the same structure of the state record
```


## Init
Let's initialise our example model. Each picker model(or state) could be
initialised with [`initMinimalModel`][initMinimalModel] and [`setOptions`][setOptions]


```elm
exampleInit : () -> ( ExampleModel, Cmd ExampleMsg )
exampleInit flags
    = ( { firstPickerModel -- for hour value
              = ScrollPicker.initMinimalModel "firstScrollPicker"
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
                   -- ^ a bit more quicker to check

        , secondPickerModel -- for minute value
              = ScrollPicker.initMinimalModel "secondScrollPicker"
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

        , messageMapWith
            = ScrollPickerMessage
          -- ^ a map function to wrap the picker messages into the ExampleMsg
        , pickerDirection = ScrollPicker.Vertical
        }              
```

## Update

In your own update function, you might need to check picker Id and update
the matched picker state(or model) accordingly.

```elm
exampleUpdate : ExampleMsg -> ExampleModel -> ( ExampleModel, Cmd ExampleMsg )
exampleUpdate msg model
    = let update
              = ScrollPicker.updateWith model
      in
          case msg of
              ScrollPickerMessage idString pickerMsg ->
                  case idString of
                      "firstScrollPicker" ->
                          let ( firstPickerModel, cmd )
                                  = update pickerMsg model.firstPickerModel

                              newModel
                                  = { model |
                                      firstPickerModel
                                          = firstPickerModel
                                    }
                                  
                          in ( case ScrollPicker.anyNewOptionSelected pickerMsg
                               of
                                   Just option ->
                                       { newModel |
                                         hourValue = option.value
                                       }
                                   Nothing ->
                                       newModel
                               , cmd
                             )

                      "secondScrollPicker" ->
                          let ( secondPickerModel, cmd )
                                  = update pickerMsg model.secondPickerModel

                              newModel
                                  = { model |
                                      secondPickerModel
                                          = secondPickerModel
                                    }
                                  
                          in ( case ScrollPicker.anyNewOptionSelected pickerMsg
                               of
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
```

And picker model itself *does not* hold selected 'option', you also need to
check some message `Elmnt.BasePickerModel.ScrollPickerSuccess`

**Note:** `anyNewOptionSelected` function can be useful to set new value
from the picker. you can also check this out from below code.

## View

Here is an example

```elm
exampleView : ExampleModel -> Html ExampleMsg
exampleView model
    = let
        theme
            = defaultTheme

        pickerHelper
            = viewScrollPicker model theme

   in
       layout [ Background.color (theme.palette.on.surface -- use same color as shade
                                      |> theme.palette.toElmUiColor)
              ] <|
           column [ centerX
                  , centerY
                  ]
               [ row [ spacing 1
                     ]
                     [ pickerHelper model.firstPickerModel
                     , pickerHelper model.secondPickerModel
                     ]

               , el [ MAttr.paddingTop 20
                    , Font.size
                        ( ScrollPicker.defaultFontSize
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
                       (model.hourValue |> String.fromInt) ++ ":" ++
                       (model.minuteValue |> String.fromInt )
           ]
```

## Subscriptions

Scroll picker relies on animation by using elm-style-animation and subscriptions is
essential to see actual animation on going now.

```elm
exampleSubscriptions : ExampleModel -> Sub ExampleMsg
exampleSubscriptions model
    = model |>
      subscriptionsWith
      [ model.firstPickerModel
      , model.secondPickerModel
      ]
```

## Main

Finally you can make main funciton with all the function above.

```elm
main : Program () ExampleModel ExampleMsg
main
    = Browser.element
      { init = exampleInit
      , view = exampleView
      , update = exampleUpdate
      , subscriptions = exampleSubscriptions
      }
```

# Testing Environment

I'm a chef and but still using Linux since 2001. I don't have enough chance
to check on Apple product.

- Firefox (currently 94.0.1) on Arch linux
- Vivaldi
- [`Gnome Web Epiphany`](https://apps.gnome.org/en-GB/app/org.gnome.Epiphany/)


# More Information
**Why elm-style-animation?** [`elm-style-animation`](/packages/mdgriffith/elm-style-animation/latest)
is not quite designed for low level animation. but you could use the module
for any other css-style based animation.

[`elm-animation`](/packages/mgold/elm-animation/latest) was also considered,
and *it is* pretty straight-foward.
However the module cannot live together in the same application due to name
colision.
