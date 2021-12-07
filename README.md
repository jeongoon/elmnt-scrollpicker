# An Elm-Ui frienly Scroll Picker

`elmnt-scrollpicker` provides an scroll picker with some animation. `elmnt`
is stands for [`Element`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
so you can use the View(widget) as an element in elm-ui.

# Tutorial

## Import

```elm
import Elmnt.BaseScrollPicker as ScrollPicker
```

## View
exampleView shows how to reveal the model on the page by using elm-ui.

check out which settings you can change [`scrollPickerDefaultTheme`](/packages/Elmnt/BaseScrollPicker#scrollPickerDefaultTheme)

```elm
exampleView : ExampleModel -> Html ExampleMsg
exampleView model
    = let
        theme
            = scrollPickerDefaultTheme

        picker
            = scrollPicker model theme

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
Unfortuneately, some states required to store in a separate record. so
you might need to declare your own Msg

```elm

    type ExampleMsg
        = ScrollPickerMessage String (Msg Int ExampleMsg)
```

*ScrollPickerMessage* is an wrapper function (or map function) to
create messages which is compatible to your own module message.

Let's say we are making a simple time picker, we need two seprate
pickers for hour and minute value.


```elm
type alias ExampleModel -- which is your own model
    = { firstPickerModel  : MinimalModel Int ExampleMsg
      , secondPickerModel : MinimalModel Int ExampleMsg
      , messageMapWith    : String -> (Msg Int ExampleMsg) -> ExampleMsg
      , pickerDirection   : Direction -- Horizontal or Vertical
      }
```

[`MinimalModel`](/packages/jeongoon/elmnt-scrollpicker/latest/Elmnt/BaseSCrollPicker#MinimalModel) is an *minimal* model for each picker. If you need more advanced features,
you can still use any other API in the module to work with your own *picker*
model As most of API use partial record type. for example [`setOptions`][setOptionsJson]
function has the definition like below

[setOptions] : /packages/jeongoon/elmnt-scrollpicker/latest/Elmnt/BaseSCrollPicker#setOptions

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

[initMinimalModel] : /packages/jeongoon/elmnt-scrollpicker/latest/Elmnt/BaseSCrollPicker#initMinimalModel
[setOptions] : /packages/jeongoon/elmnt-scrollpicker/latest/Elmnt/BaseSCrollPicker#setOptions

```elm
exampleInit : () -> ( ExampleModel, Cmd ExampleMsg )
exampleInit flags
    = ( { firstPickerModel -- for hour value
              = initMinimalModel "firstScrollPicker"
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

        , secondPickerModel -- for minute value
              = initMinimalModel "secondScrollPicker"
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
        }              
```

## Update

In your own update function, you might need to check picker Id and update
the matched picker state(or model) accordingly.

```elm
exampleUpdate : ExampleMsg -> ExampleModel -> ( ExampleModel, Cmd ExampleMsg )
exampleUpdate msg model
    = let update
              = updateWith model
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
                          let ( secondPickerModel, cmd )
                                  = update pickerMsg model.secondPickerModel

                              newModel
                                  = { model |
                                      secondPickerModel
                                          = secondPickerModel
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
                       (model.hourValue |> String.fromInt) ++ ":" ++
                       (model.minuteValue |> String.fromInt )
           ]
```

## Subscriptions

Scroll picker relies on animation by using elm-style-animation and subscriptions is
essential to see actual animation on going now.

# More Information
**Why elm-style-animation?** [`elm-style-animation`](/packages/mdgriffith/elm-style-animation/latest)
is not quite designed for low level animation. but you could use the module
for any other css-style based animation.

[`elm-animation`](/packages/mgold/elm-animation/latest) was also considered,
and *it is* pretty straight-foward.
However the module cannot live together in the same application due to name
colision.
