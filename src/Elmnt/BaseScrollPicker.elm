module Elmnt.BaseScrollPicker
        exposing ( BasicOption
                 , BasicOptionLike
                 , OptionItem
                 --, OptionSubId
                 , Direction (..)
                 , WhichViewport (..)
                 , StartEnd (..)
                 , BasicState
                 , BasicStateLike
                 , Msg (..)
                 , Error (..)
                 , MinimalPaletteLike
                 , MinimalPaletteOnLike
                 , BaseTheme
                 , BaseThemeLike
                 , defaultTheme
                 --, defaultFontSize
                 --, defaultBorderWidth
                 , BaseSettings
                 , getPosAndLengthAccessors
                 , asOptionSubId
                 , wrapOption
                 , getOptions
                 , setOptions
                 , hasOption
                 , replaceOption
                 , setScrollStopCheckTime
                 , unsafeSetScrollCheckTime
                 , getOptionIdString
                 , anyNewOptionSelected
                 , initBasicState
                 , initCmdWith
                 , viewAsElement
                 , viewAsElementHelper
                 , updateWith
                 , subscriptionsWith
                 , alwaysGotoOptionWithIdHelper
                 , alwaysGotoOptionWithIndexHelper

                 -- v  low-level api
                 , unwrapOption
                 , isSnapping
                 , stopSnapping
                 , hasCenterOption
                 , setCenterOption
                 , resetCenterOption
                 , initPseudoAnimStateHelper
                 , initPseudoAnimState
                 --, defaultShadeLengthWith
                 , defaultShadeAttrsWith
                 , defaultBaseSettingsWith
                 , scrollPosPropertyName
                 , scrollPosProperty
                 , getOptionsWrapped
                 , Geom
                 , TwoDim
                 , makeTwoDim
                 , getCenterPosAndLengthOfHelper
                 , taskGetElement
                 , taskGetViewport
                 , toMilliPixel
                 , fromMilliPixel
                 , subscriptionsWithHelper
              )

{-| This module is an implementation of picker by scrolling and basic view type
 is [`elm-ui`][elm-ui]. and animation can be done a bit tricky but easily thanks
 to [`elm-style-animation`][elm-style-animation].
Due to some non-standard way to hiding scrollbar, [`elm-css`][elm-css]
is also required.

Main difference between this picker and [`MinimalScrollPicker`] is that
 this module use an separate view for scrolling and rendering.

In result some pros are

    - no need to assume scrolling from the animation module generated or user scroll
    - better animation control especially when animation envolves css transform

[elm-ui]: /packages/mdgriffith/elm-ui/latest
[elm-css]: /packages/rtfeldman/elm-css/latest
[elm-style-animation]: /packages/mdgriffith/elm-style-animation/latest
[example]: https://github.com/jeongoon/elmnt-scrollpicker/tree/3.0.0/examples/BaseScrollPickerExample.elm

# Type

@docs BasicState, BasicOption, OptionItem, Direction, StartEnd, Msg, Error

# State(picker model) Creation, Modification and Query

@docs initBasicState, setOptions, getOptions, setScrollStopCheckTime,
anyNewOptionSelected

# Update

@docs updateWith

# Subscriptions

@docs subscriptionsWith

# View

@docs viewAsElement, defaultTheme, BaseTheme, BaseSettings

# Helper functions

@docs getOptionIdString, wrapOption, asOptionSubId

# Low-level Data types and functions

@docs BasicStateLike, BasicOptionLike, MinimalPaletteLike,
MinimalPaletteOnLike, getOptionsWrapped,
isSnapping, stopSnapping, hasCenterOption, setCenterOption, resetCenterOption,
unsafeSetScrollCheckTime, scrollPosProperty, scrollPosPropertyName,
initPseudoAnimState, initPseudoAnimStateHelper,
defaultShadeAttrsWith, defaultBaseSettingsWith, Geom, getCenterPosOf,
partitionOptionsHelper, getRelPosOfElement,
taskGetViewport, taskSetViewportHelper,
taskTriggerGetCenterOptionHelper,
alwaysUpdateOptionLengthsHelper, toMilliPixel, fromMilliPixel,
 subscriptionsWithHelper

# FIXME

    - add keyboard input support

-}

import Dict                                     exposing ( Dict )
import Array
import Set                                      exposing ( Set )
import Time
import Process
import Task                                     exposing ( Task )

import Element                                  exposing ( .. )
-- ^ this module is basically based on the elm-ui

import Element.Font             as Font
import Element.Input            as Input
import Element.Border           as Border
import Element.Background       as Background

import Color                                    exposing ( Color )
-- ^ this is best choice for color data 'period'

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

import Elmnt.Theme              as Theme
import Internal.Util            as Util
import Internal.MoreAttributes  as MAttr
import Internal.ColorUtil       as ColorUtil
import Internal.Palette         as Palette      exposing ( Palette
                                                         , PaletteWith
                                                         )
import Internal.VirtualViewport as Virtual

-- ^ an example palette

{-| General prototype for option which has minimal set of fields
 -}
type alias BasicOptionLike optionWith vt msg
    = { optionWith |
        idString        : String
      , index           : Int
      , value           : vt
      , element         : Element msg
      , surfaceLength   : Maybe Float
      }

{-| Option record for each item in the list from which user will choose.

This record depends on the type of value and element (Element)
-}
type alias BasicOption vt msg
    = BasicOptionLike {} vt msg
                     -- ^ no extra option filed.


{-| A container for option types, which is useful when you try to
put some option that has type of superset of `BasicOptionLike`
you can wrap it with [`wrapOption`](#wrapOption) unwrap with
[`unwrapOption`](#unwrapOption)

```elm
{ state |
  optionIdToItemDict
      = Dict.fromList
        [ wrapOption (YourOptionConstructor id index value element) ]
        -- or
        --  [ wrapOption { id = idValue
        --               , index = indexValue
        --               , vlaue = valueValue
        --               , element = elementValue } ]

}
...
```
-}
type OptionItem optExtra vt msg
    = OptionItem (BasicOptionLike optExtra vt msg)


{-| [`OptionItem`](#OptionItem) is an opaque type so you need
 wrapper and unwrapper
-}
wrapOption : BasicOptionLike optExtra vt msg ->
             OptionItem optExtra vt msg
wrapOption option
    = OptionItem option


{-| unwrapper for [`OptionItem`](#OptionItem). this might be useuful only
when you are making derived version of this module.
-}
unwrapOption : OptionItem optExtra vt msg -> BasicOptionLike optExtra vt msg
unwrapOption (OptionItem option)
    = option


{-| Picker direction
-}
type Direction
    = Horizontal
    | Vertical


{-| this type is for general use, and also used in the picker shading part
from the beginning and the end.
-}
type StartEnd
    = Start
    | End


{-| geometry data type which can be seen in 'Browser.Dom.Viewport'
-}
type alias Geom
    = { x : Float
      , y : Float
      , width : Float
      , height : Float
      }


{-| not exported
-}
rectangleToGeom : { width: Float, height : Float } ->
                  Geom
rectangleToGeom rect
    = { x = 0
      , y = 0
      , width
          = rect.width
      , height
          = rect.height
      }

{-| two dimensional value
-}
type alias TwoDim
    = { startPoint : Float
      , endPoint   : Float
      }

{-| set TwoDim value to ensure that `startPoint` < `endPoint`
-}
makeTwoDim : Float -> Float -> TwoDim
makeTwoDim a b
    = if a < b then
          { startPoint = a
          , endPoint   = b
          }
      else
          { startPoint = b
          , endPoint   = a
          }



{-| not exported

for argument of some type in Msg. you can see on `FindCenterOptionWith`
`CheckInitialTargetOption` and `DetermineTargetOption` etc

the function takes dom "id", and some longitudinal position in the picker
 which are center position of frame, current viewport position and lastly
ceneter position of target option.
-}
type alias OnSuccessFunction optExtra vt msg
    = String -> Float -> Float -> Float ->
      Msg optExtra vt msg


-- -- -- MODEL -- -- --

{-| Provide Minimal model (or state) to work with. most of funciton in
this module works well with your own record type generally, as
I used more generic type constraint in function definition

    ..
    , pseudoAnimState : Animation.Messenger.State msg
    ..

**Note:** elm-style-animation module doesn't supply low-level functions
to get intermediate states of animation so I need more research
but now I'm using renderPairs function to get the states of current
values in 'String' format which will be traslated into number.
Even though one state value is used, I need to use Animation.style function
to generate the state which can contain a lot more information

-}
type alias BasicState optExtra vt msg
    = BasicStateLike {} optExtra vt msg
                    -- ^ {} : no extra state field.


{-| Used for internal type checking -}
type alias BasicStateLike statExtra optExtra vt msg
    = { statExtra |
        idString                : String
      , optionIds               : List String
      , optionLengths           : List Float -- surface lengths of options
      , optionIdToItemDict      : Dict String (OptionItem optExtra vt msg)
      , targetIdString          : Maybe String
      , pseudoAnimState         : Animation.Messenger.State msg
      -- ^ elm-style-animation doesn't support low-level functions
      --   so we need to make a pseudo `style' record

      , lastScrollClock         : Time.Posix
      , lastSceneLength         : Float
      , virtualControlPos       : Virtual.Pos Virtual.SanityUnknown
                                              Virtual.SanityOk
      , virtualControlSettings  : Virtual.Settings Virtual.SanityOk
      , scrollTraceMP           : Set Int
      , finalTargetScrollPosMP  : MilliPixel
      , scrollStopCheckTime     : Int
      , optionIdInTheCenter     : Maybe String
      , frameCenterPos          : Float
      , optionCenterRelPos      : Float
      , optionLengthInfoStatus  : OptionLengthInfoStatus
      , testingValue            : String
      }


{-| Msg chain generally can break into two group

**Initialising viewports**

```
1. sync virtual viewport first
2. update option elements information including the length of each option
3. sync viewport position
4. sync *real* viewport position and *virtual* viewport position
```

**Snapping the selected item in the middle of the frame **

```
1. Detect any scroll which has delayed 'Cmd' to check
   whether scrolling is stopped or not.
2. If scroll stopped, trigger snapping to nearst item.
3. Find and decide which item will be target to snap.
4. Do animation related to snapping
5. Inform the snapping is done so outside world is able
   to know the which item(Option) is selected.
```

Unfortunately we need own Msg here which means you might need to
map over those message into your own Msg type.

There are examples in this module regarding message mapping
you could possibly search keyword 'messageMap' where I need to map the
`Msg' into `msg'
-}
type Msg optExtra vt msg
    = InitViewportsMsg          InitViewportsChain
    | OnScroll
    | SyncVirtualControlPos     Time.Posix Float Float
    | SyncLastScroll            Time.Posix Float Bool
-- ^ sync and initialising
    | OnKey                     String
    | FindCenterOptionWith      Float
                                -- ^ the center position of the control frame
                                Float
                                -- ^ viewport offset of control frame
                                Float
                                -- ^ viewport offset of center of control frame

                                (OnSuccessFunction optExtra vt msg)
    | TriggerSnapping           Time.Posix
    | SyncCenterOption          String Float Float Float
                                --^ id, base, _  , rel pos  --  _ : not used
    | SetSnapToTargetOption     String Float Float Float
                                --^ id, _  , start, rel pos
    | GotoTargetOption          String
    | ScrollPickerSuccess       (BasicOptionLike optExtra vt msg)
    | ScrollPickerFailure       String String Error
    | Animate                   Animation.Msg
    | SetViewport               SelectedViewport MilliPixel
    | NoOp


{-| Sub Msg for initializing process.
-}
type InitViewportsChain
    = UpdateOptionLengths       (Maybe Int)
    | SyncOptionLengths         (List (String, Float))
    | SyncViewportForView       Float Float


{-| this picker need to load each option lengths to make control viewport
and to search the target option to go
 -}
type OptionLengthInfoStatus
    = OptionLengthUnknown
    | OptionLengthReading
    | OptionLengthUpdated


{-| Error used for Task _x_ a

**XXX:** this module doesn't really analyse the error status very well,
but those data types are explaining the stauts in the code instead of
any other types of comments.
-}
type Error
    = NoOptionAvailable
    | InvalidOptionId (Maybe String)
    | DomError Browser.Dom.Error
    | PickerControllNotReady
    | CenterOptionUnavailable
    | ErrorMessageOnly
    | UnknownError
    | ThisIsBug
    | GetOptionLengthFailure (Maybe String) -- id string



{-| a type for getting picker direction and end point (at the beginning or the
end) and give a list of Element.Attribute used in [`BaseTheme`](#BaseTheme)
and partially used in defaultShadeAttrsWith.
-}
type alias ShadeAttrsFunction msg
    = Direction -> StartEnd -> List (Attribute msg)

{-| Only some palette color are used in this module and this is minimum
set of elements.

Please have a look at [`MinimalPaletteOn`](#MinimalPaletteOn) as
well
-}
type alias MinimalPaletteLike palette paletteOn
    = { palette |
        accent            : Color
      , surface           : Color
      , background        : Color
      , on : MinimalPaletteOnLike paletteOn
      , toElmUiColor      : Color -> Element.Color
      }


{-| Only some palette color are used in this module. and this on 'on'
field of [`MinimalPalette`](#MinimalPalette)
-}
type alias MinimalPaletteOnLike paletteOn
    = { paletteOn |
        background : Color
      , surface    : Color
      }


{-| A prototype for [`BaseTheme`](#BaseTheme)
-}
type alias BaseThemeLike baseThemeWith palette msg
    = { baseThemeWith |
        palette           : palette
      , borderWidth       : Theme.Value Int
      , borderColorFn     : Theme.Value (palette -> Color)
      , shadingColorFn    : Theme.Value (palette -> Color)
      , focusColorFn      : Theme.Value (palette -> Color)
      , backgroundColorFn : Theme.Value (palette -> Color)
      , fontColorFn       : Theme.Value (palette -> Color)
      , fontSize          : Theme.Value Int
      , shadeLength       : Theme.Value Int
      , paddingLength     : Theme.Value Int
      , pickerLength      : Theme.Value Int
      , pickerWidth       : Theme.Value Int
      , shadeAttrsFn      : Theme.Value (ShadeAttrsFunction msg)
      }

{-| An example settings value type in use here
-}
type alias BaseTheme palette msg
    = BaseThemeLike {} palette msg


{-| All setting values are set to Theme.Default, which can be applied to
scrollPicker function.

```elm
...
exampleView model
    = let
        theme
            = defaultTheme
        picker
            = viewAsElement model theme

...
-}
defaultTheme : BaseTheme Palette msg
defaultTheme
    = let
        df = Theme.Default
      in
          { palette
                = Theme.defaultPalette
          , borderWidth       = df
          , borderColorFn     = df
          , shadingColorFn    = df
          , focusColorFn      = df
          , backgroundColorFn = df
          , fontColorFn       = df
          , fontSize          = df
          , shadeLength       = df
          , paddingLength     = df
          , pickerLength      = df
          , pickerWidth       = df
          , shadeAttrsFn      = df
          }


{-| not exported
-}
defaultFontSize : Int
defaultFontSize
    = Util.sizeScaled 8

{-| not exported
-}
defaultBorderWidth : Int
defaultBorderWidth
    = 3

{-| When getting dom `id` of picker or making view of picker, this type
will indicate which is being refered.
-}
type WhichViewport
    = ForControl
    | ForView

{- FIXME: I think this is not needed anymore
-}
type SelectedViewport
    = SelectViewport WhichViewport
    | SelectBothViewport

{-| Takes the direction of picker and gives the shade length
-}
defaultShadeLengthWith : Direction -> Int
defaultShadeLengthWith pickerDirection
    = case pickerDirection of
          Horizontal ->
              Util.sizeScaled 12
          Vertical ->
              Util.sizeScaled 8

{-| Helper function for shade elm-ui attributes (List Element.Attribute)
-}
defaultShadeAttrsWith : (BaseThemeLike extraTheme
                             (MinimalPaletteLike palette
                                  (MinimalPaletteOnLike paletteOn)) msg
                        ) ->
                        (ShadeAttrsFunction msg)

defaultShadeAttrsWith theme pickerDirection startEnd
    = let
        ( attachedTo, gradientTo )
              = case ( pickerDirection, startEnd ) of
                    ( Vertical, Start ) ->
                        ( "top", "bottom" )
                    ( Vertical, End ) ->
                        ( "bottom", "top" )
                    ( Horizontal, Start ) ->
                        ( "left", "right" )
                    ( Horizontal, End ) ->
                        ( "right", "left" )

        { lengthSetter, widthSetter, shadeLength, borderWidth, pickerWidth }
           = defaultBaseSettingsWith theme pickerDirection

        pickerWidthSetter
            = fill |> maximum pickerWidth |> minimum 15 |> widthSetter

        -- helper function to change alpha value in Element.Color
        rgbaStrHelper alpha colour
            = colour
            |> ColorUtil.replaceAlpha alpha
            |> ColorUtil.rgbaStringFromColor

        backgroundImageStyle directionRawString shadeColour
            = "linear-gradient(" ++
              ( String.join ", "
                    [ directionRawString -- "to bottom" or 180deg ...
                    , shadeColour
                        |> rgbaStrHelper 1
                    , shadeColour
                        |> rgbaStrHelper 0.8
                    , shadeColour
                        |> rgbaStrHelper 0.1
                    , shadeColour
                        |> rgbaStrHelper 0.05
                    ]
              ) ++ ")"

      in
          [ MAttr.positionAbsolute
          , MAttr.passPointerEvents
          , MAttr.style "backdrop-filter" "blur(3px)"
          , (shadeLength - borderWidth)  |> px |> lengthSetter
          , pickerWidthSetter

          , Border.width 0
          , Border.color
              ( theme.palette
                    |> ( theme.borderColorFn
                          |> Theme.withDefault ( .surface )
                       )
                    |> theme.palette.toElmUiColor
              )
          , MAttr.style attachedTo "0px"
          , MAttr.style "background-image" <|
              backgroundImageStyle ("to " ++ gradientTo)
                  <| ( theme.palette
                        |> (  theme.shadingColorFn
                                |> Theme.withDefault ( .surface )
                           )
                     )
          ]


{-| Settings generated from the picker [`Direction`](#Direction) for function
such as 'viewAsElement' and 'defaultShadeAttrsWith'.
-}
type alias BaseSettings cssCompat msg
    = { lengthSetter            : Length -> Attribute msg
      , widthSetter             : Length -> Attribute msg
      , longitudinalContainer   : List (Attribute msg) -> List (Element msg) ->
                                  Element msg
      , ancherString            : String
      , windowEdges             : { top    : Int
                                  , right  : Int
                                  , bottom : Int
                                  , left   : Int
                                  }
      , centerLateral           : Attribute msg
      , cssWidthSetter          : Css.LengthOrAuto cssCompat -> Css.Style
      , cssOverFlowLongitudinal : Css.Overflow cssCompat -> Css.Style
      , cssOverFlowLateral      : Css.Overflow cssCompat -> Css.Style
      , fontSize                : Int
      , shadeLength             : Int
      , paddingLength           : Int
      , borderWidth             : Int
      , pickerLength            : Int
      , pickerWidth             : Int
      }


{-| not exported

when make Float -> Int pixel value, `trucate` is normally working good
 enough for FF, Chrome, Webkit.
 -}
cut_ : Float -> Int
cut_ 
    = truncate

{-| not exported
-}
defaultPaddingLengthFrom : Int -> Int
defaultPaddingLengthFrom shadeLength
    = (shadeLength |> toFloat) * 1.5 |> cut_


{-| not exported
-}
defaultPaddingLengthWith
    : { theme |
        shadeLength   : Theme.Value Int
      , paddingLength : Theme.Value Int
      } ->
      Direction ->
      Int

defaultPaddingLengthWith theme pickerDirection
    = let
        shadeLength
            = theme.shadeLength
            |> Theme.withDefault
               ( defaultShadeLengthWith pickerDirection )

   in
       theme.paddingLength
         |> Theme.withDefault
            ( defaultPaddingLengthFrom shadeLength )


{-| Generate setting values for a picker which has `Direction`
-}
defaultBaseSettingsWith : { theme |
                            fontSize      : Theme.Value Int
                          , borderWidth   : Theme.Value Int
                          , shadeLength   : Theme.Value Int
                          , paddingLength : Theme.Value Int
                          , pickerLength  : Theme.Value Int
                          , pickerWidth   : Theme.Value Int
                          } ->
                          Direction ->
                          BaseSettings compatible msg

defaultBaseSettingsWith theme pickerDirection
    = let
        borderWidth
            = theme.borderWidth
            |> Theme.withDefault defaultBorderWidth

        shadeLength
            = theme.shadeLength
            |> Theme.withDefault
               ( defaultShadeLengthWith pickerDirection )

        paddingLength
            = theme.paddingLength
            |> Theme.withDefault
               ( defaultPaddingLengthFrom shadeLength )

        fontSize
            = theme.fontSize
            |> Theme.withDefault defaultFontSize

        pickerWidth -- lateral length of Element
            = theme.pickerWidth
            |> Theme.withDefault pickerLength

        pickerLength
            = theme.pickerLength
            |> Theme.withDefault
               ( (shadeLength |> toFloat) * 3.5
                   |> cut_ )

   in
       ( case pickerDirection of
             Horizontal ->
                 BaseSettings
                     width height row "left"
                     { top = 0, bottom = 0
                     , left = borderWidth, right = borderWidth
                     }
                     centerY
                     Css.height Css.overflowX Css.overflowY

             Vertical ->
                 BaseSettings
                     height width column "top"
                     { top = borderWidth, bottom = borderWidth
                     , left = 0, right = 0
                     }
                     centerX
                     Css.width Css.overflowY Css.overflowX
       )
       -- and rest settings ...
       fontSize shadeLength paddingLength borderWidth pickerLength pickerWidth


{-| property name used for Animation which will be sent to
"Browser.Dom.setViewportOf' eventually
-}
scrollPosPropertyName : String
scrollPosPropertyName = "spos"

{-| Function to make actuall property for scroll animation
-}
scrollPosProperty : Float -> Animation.Property
scrollPosProperty spos
    = Animation.custom scrollPosPropertyName  spos ""

{-| scroll position consts of one dimension, if initPseudoAnimStateHelper is
given with property function, it will initialise Animation
-}
initPseudoAnimStateHelper : (a -> Animation.Property) ->
                            a ->
                            Animation.Messenger.State msg
initPseudoAnimStateHelper propertyFn val
    = Animation.style
      [ propertyFn val ]


{-| combine scrollPosProperty function and initPseudoAnimStateHelper
to get simpler setter for animation scroll property
-}
initPseudoAnimState : Float ->
                      Animation.Messenger.State msg
initPseudoAnimState
    = initPseudoAnimStateHelper scrollPosProperty


{-| Helper type aliasing for changing some state -}
type alias BasicStateUpdater statExtra optExtra vt msg
    = BasicStateLike statExtra optExtra vt msg ->
      BasicStateLike statExtra optExtra vt msg


-- -- -- Helper functions for user -- -- --

{-| get position accessor and length accessor depends on the direction
of picker

```elm
( posAccesssor, lengthAccessor )
  = getPosAndLengthAccessor pickerDirection
```
-}
getPosAndLengthAccessors : Direction ->
                           ( (Geom -> Float), (Geom -> Float) )
getPosAndLengthAccessors pickerDirection
    = case pickerDirection of
          Horizontal ->
              ( .x, .width )

          Vertical ->
              ( .y, .height )


{-| get a list of option as OptionItem
-}
getOptionsWrapped : BasicStateLike statExtra optExtra vt msg ->
                    List (OptionItem optExtra vt msg)
getOptionsWrapped { optionIds, optionIdToItemDict }
    =  optionIds
    |> List.map
       ( optionIdToItemDict
           |> (Util.flip Dict.get)
       )
    |> List.filterMap identity


{-| get a list of option record data from the whole options by searching
option ID in a Dict.

The order of options in the same one of optionID list.
-}
getOptions : BasicStateLike statExtra optExtra vt msg ->
             List (BasicOptionLike optExtra vt msg)

getOptions state
    = getOptionsWrapped state
    |> List.map
       (\(OptionItem bare) -> bare)


{-| Save options from the list of pair of sub-id-string for option, details

details includes at least
    - value
    - element

this module still provides low-level api, so to make Option Item
 you might need to something like below

```elm
    let emptyOption =
          { idString = ""
          , index    = -1
          , surfaceLength
                     = Nothing
          -- ^ dummy values
          , value    = -1
          , element  = Element.none
          }
    in
       initBasicState "myPicker"
          |> setOptions
             [ ( asOptionSubId "1", ScrollPicker.wrapOption
                     { emptyOption |
                       value
                           = 1
                     , element
                           = Element.text "1"
                     }
               )
             , ( asOptionSubId "2", ScrollPicker.wrapOption
                      { emptyOption |
                        value
                           = 2
                      , element
                           = Element.text "2"
                      }
                )
           ... -- probably List.map is more convenient.
             ]
-}
setOptions : List ( OptionSubId, OptionItem optExtra vt msg ) ->
             BasicStateUpdater statExtra optExtra vt msg

setOptions subIdToOptionPairs state
    = let
        optionIds
            = subIdToOptionPairs
            |> List.map
               (\(subIdString, _) ->
                    getOptionIdString state.idString subIdString
               )

        indexedOptionIds
            = optionIds
            |> List.indexedMap Tuple.pair

        optionIdToItemDict
            = (indexedOptionIds, subIdToOptionPairs)
            |> (Util.uncurry <| List.map2
                    (\(index, idString) (_, (OptionItem option)) ->
                         ( idString
                         , OptionItem
                               { option | -- update to correct value
                                 idString
                                     = idString
                               , index
                                     = index
                               }
                         )
                    )
               )
            |> Dict.fromList

        state1
            = state |> resetPickerControl
   in

       { state1 |
         optionIds
             = optionIds

       , optionIdToItemDict
             = optionIdToItemDict
       }


{-| check given option item is in the state.
-}
hasOption : OptionItem optExtra vt msg ->
            BasicStateLike statExtra optExtra vt msg ->
            Bool
hasOption (OptionItem option) state
    = state.optionIds
    |> List.member option.idString

{-| replace option item if it exists in the state.

function for adding option(s) is not available.
please use arrange your options
and use [`setOptions`](#setOptions) manually.
-}
replaceOption : OptionItem optExtra vt msg ->
                BasicStateUpdater statExtra optExtra vt msg

replaceOption ((OptionItem option) as optionItem) state
    = if state
          |> hasOption optionItem then

          { state |
            optionIdToItemDict
                = state.optionIdToItemDict
                |> Dict.insert
                   option.idString optionItem
          }

      else
          -- newly insertion not allowed here
          state


{-| Every scroll is being watched to check whether it is stopped at the moment
and this function will change the timing to wait until checking.

**Limitation:** minimum value is 75 (ms). Animation will fail or work
unexpectedly under 75 ms.
-}
setScrollStopCheckTime : Int ->
                         { stateHas |
                           scrollStopCheckTime  : Int
                         } ->
                         { stateHas |
                           scrollStopCheckTime  : Int
                         }

setScrollStopCheckTime milliSeconds
    = unsafeSetScrollCheckTime (min 75 milliSeconds)


{-| You can test any value -- even under 75 ms
-- however which is not recommended
-}
unsafeSetScrollCheckTime : Int ->
                           { stateHas |
                             scrollStopCheckTime : Int
                           } ->
                           { stateHas |
                             scrollStopCheckTime : Int
                           }

unsafeSetScrollCheckTime milliSeconds state
    = { state |
        scrollStopCheckTime
            = milliSeconds
      }

{-| we will only interested in the scroll happened in control viewport,
this function will give `id` for the viewport.
-}
getControlIdString : { stateHas | idString : String
                     } ->
                     String
getControlIdString state
    = "ctl-" ++ state.idString

{-| make option id string value for 'option.idString' which will be
useful if you want to access the id on the page.
-}
getOptionIdString : String ->
                    OptionSubId ->
                    String

getOptionIdString pickerIdString (OptionSubId optionSubIdString)
    = let
        concator
            = "~"
   in
       pickerIdString ++ concator ++ optionSubIdString

{-| Check the Msg, and return if there is any new selected option

please check this [`Example`][example] to see how you could use of it.
-}
anyNewOptionSelected : Msg optExtra vt msg ->
                       Maybe (BasicOptionLike optExtra vt msg)
anyNewOptionSelected msg
    = case msg of
          ScrollPickerSuccess option ->
              Just option
          _ ->
              Nothing


{-|
minimal testing function if the picker is snapping to some item
at the moment
-}
isSnapping : BasicStateLike statExtra optExtra vt msg ->
             Bool

isSnapping state
    = case state.targetIdString of
          Just _ ->
              True
          _ ->
              False


{-|
reset some states for runtime to stop snapping
which includes current target, scroll position to snap to, clock when scroll
happened etc.

**Note:** this function only try to stop snapping but asynchronous messasing
will produce more animation after calling this function, so keep in mind
that animation for snapping is not guaranteed to be done even if call this
function in `model' part.
-}
stopSnapping : BasicStateUpdater statExtra optExtra vt msg
stopSnapping state
    = { state |
        targetIdString
            = Nothing
      , finalTargetScrollPosMP
            = MilliPixel -1
      , scrollTraceMP
            = Set.empty
      , pseudoAnimState =
          initPseudoAnimState 0
      }

{-| check if we already know which item is is near to the center
-}
hasCenterOption : BasicStateLike statExtra optExtra vt msg ->
                  Bool
hasCenterOption state
    = case state.optionIdInTheCenter of
          Just _ ->
              True
          _ ->
              False

{-| set the option as the nearest one to the center of frame
-}
setCenterOption : String -> Float -> Float ->
                  BasicStateUpdater statExtra optExtra vt msg

setCenterOption centerOptionIdString basePos relPos state
    = { state |
        frameCenterPos
            = basePos
      , optionCenterRelPos
            = relPos
      , optionIdInTheCenter
            = Just centerOptionIdString
      }


{-| reset the value for the option in the center
-}
resetCenterOption : BasicStateUpdater statExtra optExtra vt msg
resetCenterOption state
    = { state |
        optionIdInTheCenter
            = Nothing
      }

{-| test the picker is ready to get user scrolling input
 -}
isPickerControlReady : BasicStateLike statExtra optExtra vt msg ->
                       Bool
isPickerControlReady state
    = let numOfOptions
              = state.optionIds
              |> List.length
    in
        state.optionLengthInfoStatus == OptionLengthUpdated &&
        numOfOptions > 0 &&
        numOfOptions == ( state.optionLengths |> List.length )


resetPickerControl : BasicStateUpdater statExtra optExtra vt msg
resetPickerControl state
    = { state |
        optionLengths
            = []
      , optionLengthInfoStatus
            = OptionLengthUnknown
      }

-- -- -- Helper functions for Internal usage -- -- --

{-| [`Browser.Dom.Viewport`](/packages/elm/browser/latest/Browser-Dom#Viewport),
[`Browser.Dom.Element`](/packages/elm/browser/latest/Browser-Dom#Element)
share basic record accessor like `.x`  `.y`  `.width`  `.height`

getCenterPosAndLengthHelper function try to get center poisition of the some field.

ex) to get center 'y' position of viewport, you can try

```elm
let getCenterPosAndLengthOf
        = getCenterPosAndLengthOfHelper .y .height

in aRecord
     |> getCenterPosAndLengthOf .viewport

```

will gives ( center position, length ) of `viewport` field

-}
getCenterPosAndLengthOfHelper
    : (Geom -> Float) -> (Geom -> Float) -> (rec -> Geom) -> rec ->
      (Float, Float)

getCenterPosAndLengthOfHelper posAccessor lengthAccessor geomAccessor record
    = let
        geom
            = record
            |> geomAccessor

        geomPos
            = geom
            |> posAccessor

        geomLength
            = geom
            |> lengthAccessor

   in
       ( geomPos + 0.5 * geomLength  -- center longitudinal position
       , geomLength )


{-| Task helper to get element of item with `id`
-}
taskGetElement : String ->
                 Task Error Browser.Dom.Element

taskGetElement idString
    = Browser.Dom.getElement idString
    |> Task.mapError DomError


{-| Task helper to get viewport of the item with `id`
-}
taskGetViewport : String ->
                  Task Error Browser.Dom.Viewport

taskGetViewport idString
    = Browser.Dom.getViewportOf idString
    |> Task.mapError DomError


{-| Safely determine dom `id` to set viewport and apply with it.
-}
taskSetViewportHelper
    : { appModelWith |
        messageMapWith : (String ->
                              (Msg optExtra vt msg) -> msg)
      , pickerDirection : Direction
      } ->
      WhichViewport ->
      Float ->
      { stateHas | idString : String } ->
      Task Error ()

taskSetViewportHelper { pickerDirection } whichViewport newPos state
    = let
        viewportId
            = case whichViewport of
                  ForView ->
                      state.idString
                  ForControl ->
                      state |> getControlIdString

        setViewPort
            = Browser.Dom.setViewportOf viewportId
   in
       ( case pickerDirection of
             Horizontal ->
                 setViewPort newPos 0
             Vertical ->
                 setViewPort 0 newPos
       ) |> Task.mapError DomError


{-| not exported

-}
updateOptionLengthMaxTry : Int
updateOptionLengthMaxTry
    = 100

{-| To set the target to select programatically,
you can call with Task.perform with `identity` function

```elm

let initModel
        = ...
in
   ( initModel
   , Task.perform identity
         ( yourAppModel
             |> BaseScrollPicker.alwaysGotoOptionWithIdHelper initModel
                (BaseScrollPicker.getOptionIdString
                     yourPickerState.idString
                     (BaseScrollPicker.asOptionSubId optionSubIdString)
                ) -- > or optionIdString if you know the value.

                Nothing -> ignore the any side effects or errors
         )
  )
```

-}
alwaysGotoOptionWithIdHelper
    : { appModelWith |
        messageMapWith : (String ->
                              (Msg optExtra vt msg) -> msg)
      , pickerDirection : Direction
      } ->
      String ->
      Maybe (Error -> msg) ->
      BasicStateLike statExtra optExtra vt msg ->
      Task Never msg

alwaysGotoOptionWithIdHelper appModel optionIdString mbErrorHandler state
    = let
        messageMap
            = appModel.messageMapWith state.idString
      in
          ( case mbErrorHandler of
                Just errorHandler ->
                    case state.optionIdToItemDict
                            |> Dict.get optionIdString of
                        Just _ ->
                            messageMap <|
                            GotoTargetOption optionIdString

                        Nothing ->
                            errorHandler NoOptionAvailable

                Nothing ->
                    messageMap <|
                    GotoTargetOption -- it is unsafe way,
                                     -- but this Msg will handle the error
                                     -- with `NoOp`
                    optionIdString

          ) |> Task.succeed

{-| go to option with it's index number in the list

```elm
gotoLastCmd
  = let targetIndex
        = yourPickerState
        |> BaseScrollPicker.getOptions
        |> List.length
        |> \n -> n - 1
    in
       yourPickerState
        |> BaseScrollPicker.alwaysGotoOptionWithIndexHelper
           yourModel
           targetIndex
           Nothing
        |> Task.perform idenity
```
-}
alwaysGotoOptionWithIndexHelper
    : { appModelWith |
        messageMapWith : (String ->
                              (Msg optExtra vt msg) -> msg)
      , pickerDirection : Direction
      } ->
      Int ->
      Maybe (Error -> msg) ->
      BasicStateLike statExtra optExtra vt msg ->
      Task Never msg

alwaysGotoOptionWithIndexHelper appModel optionIndex mbErrorHandler state
    = let optionIdsArray
              = state.optionIds
              |> Array.fromList

          messageMap
              = appModel.messageMapWith state.idString
      in
          Task.succeed <|
              case optionIdsArray
                     |> Array.get optionIndex of

                  Just optionIdString ->
                      messageMap <|
                      GotoTargetOption optionIdString

                  Nothing ->
                      -- out of range
                      case mbErrorHandler of
                          Just errorHandler ->
                              errorHandler NoOptionAvailable

                          Nothing ->
                              messageMap NoOp


{-| -}
type MilliPixel
    = MilliPixel Int

{-| An utility which converts an floating value to an integer value which
contains upto milli of base unit (pixel in this case)
-}
toMilliPixel : Float -> MilliPixel
toMilliPixel floatVal
    = floatVal * 1000
    |> truncate -- not cut_
    |> MilliPixel

{-| An utility which converts an integer value(which contains up to thousandth
value of original) to an float value.
-}
fromMilliPixel : MilliPixel -> Float
fromMilliPixel (MilliPixel mpInt)
    = mpInt
    |> toFloat
    |> \n -> n / 1000


-- -- -- INIT -- -- --

{-| Helper function to initialise the basic state(model). You can call
[`setOptions`](#setOptions) after this.

```elm
    let emptyOption =
          { idString = ""
          , index    = -1
          , surfaceLength
                     = Nothing
          -- ^ dummy values
          , value    = -1
          , element  = Element.none
          }
    in
       initBasicState "myPicker"
          |> setOptions
             ...
```

if you make another scroll picker
based on this module, you might consider to use
[`setInitBasicState`](#setInitBasicState) as well.

-}
initBasicState : String ->
                 BasicState optExtra vt msg

initBasicState idString
    = let
        virtualControlSettings
            = Virtual.toSettings
              { virtualPageLength
                    = Virtual.makePageLength 9000
              , wormOutPadding
                    = 1000
              , wormInMargin
                    = 800
              }

   in
       { idString
             = idString
       , optionIds
             = []
       , optionLengths
             = []
       , optionIdToItemDict
             = Dict.empty
       , targetIdString
             = Nothing
       , pseudoAnimState
             = initPseudoAnimState 0
             --   this is not actual Html Style elements
             --   just for storing some animation status
             --   `pos' is used for `Browser.Dom.setViewportOf'
       , lastScrollClock
             = Time.millisToPosix 0
       , lastSceneLength
             = -1
       , virtualControlPos
             = Virtual.emptyPosWith
               virtualControlSettings
       , virtualControlSettings
             = virtualControlSettings
       , scrollTraceMP
             = Set.empty
       , finalTargetScrollPosMP
             = MilliPixel -1
       , scrollStopCheckTime
             = 75 -- 75 ms
       , optionIdInTheCenter
             = Nothing
       , frameCenterPos
             = -1
       , optionCenterRelPos
             = -1
       , optionLengthInfoStatus
             = OptionLengthUnknown
       , testingValue
             = "init"
       }

{-| this function shows a way to init your `BasicStateLike` model
`initBasicState` value.

```elm
state
    = someEmptyYourStateLike
    |> setInitBasicState

```

-}
resetInitBasicState : String ->
                      (BasicStateLike statExtra optExtra vt msg) ->
                      (BasicStateLike statExtra optExtra vt msg)
resetInitBasicState idString state
    = let basicSt
              = initBasicState idString
      in
          { state |
            idString                = basicSt.idString
          , optionIds               = basicSt.optionIds
          , optionLengths           = basicSt.optionLengths
          , optionIdToItemDict      = basicSt.optionIdToItemDict
          , targetIdString          = basicSt.targetIdString
          , pseudoAnimState         = basicSt.pseudoAnimState
          , lastScrollClock         = basicSt.lastScrollClock
          , lastSceneLength         = basicSt.lastSceneLength
          , virtualControlPos       = basicSt.virtualControlPos
          , virtualControlSettings  = basicSt.virtualControlSettings
          , scrollTraceMP           = basicSt.scrollTraceMP
          , finalTargetScrollPosMP  = basicSt.finalTargetScrollPosMP
          , scrollStopCheckTime     = basicSt.scrollStopCheckTime
          , optionIdInTheCenter     = basicSt.optionIdInTheCenter
          , frameCenterPos          = basicSt.frameCenterPos
          , optionCenterRelPos      = basicSt.optionCenterRelPos
          , optionLengthInfoStatus  = basicSt.optionLengthInfoStatus
          , testingValue  = basicSt.testingValue
         }


{-| To distingusih between *full* dom id string and *sub* id string
`OptionSubIdString` is used.

please, use [`asOptionSubId`](#asOptionSubId) to wrap a string.
-}
type OptionSubId
    = OptionSubId String

{-| Generally used when you [`setOptions`](#setOptions)
-}
asOptionSubId : String -> OptionSubId
asOptionSubId
    = OptionSubId

{-| `initCmdWith` will make picker choose the initial option you speicify
with **sub** id string (not full id which you can access with .idString
from [`BasicOptionLike`](#BasicOptionLike) after [`setOptions`](#setOptions)

```elm
yourPickerState
  |> initCmdWith yourAppModel subOptionIdString
```
-}
initCmdWith : { appModelWith |
               messageMapWith : (String -> (Msg optExtra vt msg) -> msg)
             , pickerDirection : Direction
             } ->
             OptionSubId->
             (BasicStateLike statExtra optExtra vt msg) ->
             Cmd msg
initCmdWith ({ messageMapWith } as appModel) optionSubId state
    = [ state
          |> alwaysGotoOptionWithIdHelper appModel
             (getOptionIdString state.idString optionSubId)
             Nothing -- Maybe (error -> msg)

      , InitViewportsMsg (UpdateOptionLengths Nothing)
          |> messageMapWith state.idString
          |> Task.succeed

      ]
    |> List.map
       (Task.perform identity)
    |> Cmd.batch

-- need to perform update lengths
-- XXX: update lengths also need to wait until all the option are rendered ...

-- -- -- View -- -- --

{-| Generating Element with theme setting and state value
each function only try to some state value in the whole record
so if you can apply this funciton with additional state you might want to use.

BaseTheme DOES NOT use all the color in the Palette. the Colors used
in the theme are 'accent', 'surface', 'background' 'on.background',
 'on.surface'. as you can see in the long signature.

This means the color listed above are should be in your own palette at least,
even if you are using your own color accessor(function) with your theme.
-}
viewAsElement
    : { appModel |
        messageMapWith : (String -> (Msg optExtra vt msg) -> msg)
      , pickerDirection : Direction
      } ->
      ( BaseThemeLike extraTheme (MinimalPaletteLike pal
                                  (MinimalPaletteOnLike palOn)) msg
      ) ->
      BasicStateLike statExtra optExtra vt msg ->
      Element msg

viewAsElement appModel theme state
    = state
    |> viewAsElementHelper appModel theme Nothing


{-| To change the way the option elements are generated,
you could pass the options (list of elements) in the end.

so basically [`viewAsElement`](#viewAsElement) is
```elm
viewAsElement appModel theme state
    = state
    |> viewAsElementHelper appModel theme Nothing
                                          ^^^^^^^
```

so `viewAsElementHelper` will use default method to create them.
-}
viewAsElementHelper
    : { appModel |
        messageMapWith : (String -> (Msg optExtra vt msg) -> msg)
      , pickerDirection : Direction
      } ->
      ( BaseThemeLike extraTheme (MinimalPaletteLike pal
                                  (MinimalPaletteOnLike palOn)) msg
      ) ->
      (Maybe (List (Element msg))) ->
      BasicStateLike statExtra optExtra vt msg ->
      Element msg

viewAsElementHelper { messageMapWith, pickerDirection }
                    theme mbCustomOptionElements state
    = let
        messageMap
            = messageMapWith state.idString

        { lengthSetter, widthSetter, longitudinalContainer,
          ancherString, windowEdges, centerLateral, cssWidthSetter,
          cssOverFlowLongitudinal, cssOverFlowLateral,
          fontSize, borderWidth, shadeLength, paddingLength,
          pickerWidth, pickerLength } =

            defaultBaseSettingsWith theme pickerDirection

        getColourWithDefault defFn customFn
            = theme.palette
            |> (Theme.withDefault defFn customFn)

        getStyledColourWithDefault defFn customFn
            = getColourWithDefault defFn customFn
            |> ColorUtil.toStyledColor

        pickerWidthSetter
            = (px pickerWidth) |> minimum 15 |> widthSetter

        makeShadeAttrs startEnd
            = ( theme.shadeAttrsFn
                  |> Theme.withDefault
                     (defaultShadeAttrsWith theme)
              )
              pickerDirection startEnd

        viewPickerPaddingElement
            = el [ paddingLength |> px |> lengthSetter
                  , fill |> widthSetter
                 ] <| none
              -- ^ need some padding to centerize first and last item
              --   (or we can use padding ???)

        -- default simple viewOptions function
        viewOptions
            = mbCustomOptionElements
            |> Maybe.withDefault
               ( state
                   |> getOptions
                   |> List.map
                      (\opt ->
                           el [ opt.idString |> MAttr.id
                              , pickerLength - shadeLength * 2
                                  |> px |> lengthSetter
                              , centerLateral
                              ] <|
                           el [ centerX
                              , centerY
                              ] opt.element
                      )
               )

        viewPicker
            = longitudinalContainer
              [ fill |> widthSetter
              ]
              [ el [ fill |> widthSetter
                   , MAttr.positionAbsolute
                   , MAttr.passPointerEvents
                   ] <| viewPickerHelper ForView

              , el [ fill |> widthSetter
                   , Background.color <| rgb255 0 0 0
                   , MAttr.style "opacity" "0.5"
                   ] <| viewPickerHelper ForControl
              ]

        viewPickerHelper whichElement
            = Html.Styled.div
              ( [ css [ Css.pseudoElement "-webkit-scrollbar"
                                       -- ^ no standard
                            [ Css.display Css.none ]

                      , Css.property "scrollbar-width" "none" -- standard
                      , cssWidthSetter <| Css.pct 100
                      , Css.border <| Css.px 5
                      , Css.borderColor
                            ( theme.borderColorFn
                                |>  getStyledColourWithDefault
                                    (.on >> .background)
                            )
                      , case  whichElement of
                            ForControl ->
                                cssOverFlowLongitudinal Css.scroll
                            ForView ->
                                cssOverFlowLongitudinal Css.hidden

                      , cssOverFlowLateral Css.hidden
                      ]

                , Html.Styled.Attributes.id
                      ( case whichElement of
                            ForControl ->
                                state |> getControlIdString
                            ForView ->
                                state.idString
                      )
                ] ++
                    case whichElement of
                        ForControl ->
                            [ Html.Styled.Events.on "scroll" <|
                                  Decode.succeed <| messageMap OnScroll
                            ]
                        ForView ->
                            []
              )

              -- this isn't consistent solution but I don't want to think more
              -- about css, so I'm going back to elm-ui.
              [ ( layoutWith { options = [ Element.noStaticStyleSheet ] }
                      [ ] <|

                      longitudinalContainer
                      [ fill |> widthSetter
                      , pickerLength |> px |> lengthSetter
                      , Font.size fontSize
                      , Font.color
                            ( theme.fontColorFn
                                |> getColourWithDefault ( .on >> .surface )
                                |> theme.palette.toElmUiColor
                            )
                      ]
                      ( case whichElement of
                            ForControl ->
                                {- this doesn't look harmful
                                if state |> isPickerControlReady then
                                -}
                                    let
                                        len
                                            = state.virtualControlSettings
                                            |> Virtual.toElementLength
                                            |> cut_
                                    in
                                        [ el [ fill |> widthSetter
                                             , fill |> minimum len |> lengthSetter
                                             ] <| el [ fill |> widthSetter
                                                     , fill |> minimum len |> lengthSetter
                                                     , padding 0
                                                     -- ^ required to ensure minimum length
                                                     ] <| text "controller"
                                        ]

                                {-
                                else
                                    none
                                -}

                            ForView ->
                                [ viewPickerPaddingElement ] ++
                                viewOptions ++
                                [ viewPickerPaddingElement ]
                      )

                ) |> Html.Styled.fromUnstyled
              ]
              -- ^ Html.Styled div

              |> Html.Styled.toUnstyled
              |> Element.html

   in el [ pickerWidthSetter
         , Background.color
             ( theme.backgroundColorFn
                   |> getColourWithDefault .surface
                   |> theme.palette.toElmUiColor
             )

         , inFront <| -- shading at the beginning
             el (makeShadeAttrs Start) <| none
         -- v  window to see the selected item
         , inFront <|
             el [ MAttr.positionAbsolute
                , MAttr.passPointerEvents
                , MAttr.style ancherString <|
                    (shadeLength - borderWidth |> String.fromInt) ++ "px"
                , Border.widthEach windowEdges
                , Border.color
                    ( theme.focusColorFn
                          |> getColourWithDefault .accent
                          |> theme.palette.toElmUiColor
                    )

                , pickerWidthSetter
                , (pickerLength - (shadeLength - borderWidth) * 2)
                    |> px |> lengthSetter
                ] <| none

         , inFront <| -- shading at the end
             el (makeShadeAttrs End) <| none
         ] <| viewPicker


-- -- -- UPDATE -- -- --

{-| updateWith function needs your own app model to ask to get `messageMapWith`
 and `pickerDirection` from it. So if you want to use multiple picker,
you can keep the same information in the same place in benefit.

As other update function supposed to do, updateWith also does the job
described in the [`Msg`](#Msg) of the module.

-}
updateWith : { appModel |
               messageMapWith : (String -> (Msg optExtra vt msg) -> msg)
             , pickerDirection : Direction
             } ->
             ( BaseThemeLike extraTheme (MinimalPaletteLike pal
                                             (MinimalPaletteOnLike palOn)) msg
             ) ->
             Msg optExtra vt msg ->
             BasicStateLike statExtra optExtra vt msg ->
             ( BasicStateLike statExtra optExtra vt msg
             , Cmd msg
             )

updateWith ({ messageMapWith, pickerDirection } as appModel)
           theme msg state
    = let
        messageMap
            = messageMapWith state.idString

        getMaybeAnimProperty animState propName
            -- `elm-style-animation' doesn't seem to supply the low level
            -- accessor and pseudoAnimState has only one member so...
            -- otherwise we need to use Dict.fromList |> Dict.get ...
            = Animation.renderPairs animState
            |> Dict.fromList
            |> Dict.get propName

        ( posAccessor, lengthAccessor )
            = case pickerDirection of
                  Horizontal ->
                      ( .x, .width )
                  Vertical ->
                      ( .y, .height )

        paddingLength
            = defaultPaddingLengthWith theme pickerDirection

        getCenterPosAndLengthOf
            = getCenterPosAndLengthOfHelper posAccessor lengthAccessor

        taskGetControlCenterPosAndLength
            = state
                |> getControlIdString
                |> taskGetElement
                |> Task.andThen
                   (getCenterPosAndLengthOf .element
                        >> Task.succeed
                   )

        taskGetViewportPosAndCenterPos whichViewport
            = let
                idAccessor
                    = case whichViewport of
                          ForView ->
                              .idString
                          ForControl ->
                              getControlIdString
           in
               state
                 |> idAccessor
                 |> taskGetViewport
                 |> Task.andThen
                    (\vp ->
                         ( vp.viewport
                            |> posAccessor
                         , vp
                            |> getCenterPosAndLengthOf .viewport
                            |> Tuple.first
                         )
                         |> Task.succeed
                    )

        taskGetControlViewportPos
            = state
                |> getControlIdString
                |> taskGetViewport
                |> Task.map
                   (.viewport >> posAccessor)


        taskGetViewViewportPosAndSceneLength
            = state.idString
                |> taskGetViewport
                |> Task.map
                   (\vp ->
                        ( vp.viewport
                              |> posAccessor
                        , vp.scene
                              |> rectangleToGeom
                              |> lengthAccessor
                        )
                   )

        taskGetOptionCenterPos idstr
            = idstr
            |> taskGetElement
            |> Task.andThen
               (getCenterPosAndLengthOf .element
                    >> Tuple.first
                    >> Task.succeed
               )

        isInScrollTraceHelper (MilliPixel mpInt)
            = state.scrollTraceMP 
            |> Set.toList
            |> List.map
               -- ^ make distance list
               (\mpInt1 ->
                    abs (mpInt1 - mpInt) // 1000
                        |> (*) 1000
               -- ^ truncate as a pixel value (not milli pixel)
               )

        isInScrollTrace mp -- milli pixel
            = isInScrollTraceHelper mp
            |> List.member 0
               -- ^ and find any distance is equal to zero.

        cleanScrollPos
            = cut_
              >> toFloat

        pseudoState realViewportPos -- state for `Virtual.*`
            = { lastScrollClock
                    = state.lastScrollClock
              , lastViewportPos
                    = realViewportPos
              , lastSceneLength
                    = state.lastSceneLength
              , scrollStopCheckTime
                    = state.scrollStopCheckTime
              }

   in
       case msg of
 
           InitViewportsMsg (UpdateOptionLengths mbMaxTryCount) ->
               -- if `initCmdWith` in use, this Msg will fire automatically
               let maxTryCount
                       = mbMaxTryCount
                       |> Maybe.withDefault updateOptionLengthMaxTry
               in
                   ( { state |
                       optionLengthInfoStatus
                           = OptionLengthReading -- synced well ??
                     }

                   , if maxTryCount < 1 then
                         Task.perform messageMap <|
                         Task.succeed <|
                         ScrollPickerFailure
                         ( state.idString ++ ":>"
                               ++ "UpdateOptionLengths" )
                         "had reached max number of attempts"
                         ( GetOptionLengthFailure Nothing )

                     else
                         state.optionIds
                           |> List.map
                              ( \optIdStr ->
                                    taskGetElement optIdStr
                                      |> Task.andThen
                                         ( \domEelement ->
                                               let
                                                   optLen
                                                       = domEelement.element
                                                       |> lengthAccessor
                                               in
                                                   ( optIdStr, optLen )
                                                    |> Task.succeed
                                         )
                              ) --> List (Task Error Float)
                           |> Task.sequence
                           |> Task.attempt
                              (\res ->
                                   messageMap <|
                                   case res of
                                       Ok  optIdToLengthPairs ->
                                           optIdToLengthPairs
                                             |> SyncOptionLengths
                                             |> InitViewportsMsg

                                       Err _ ->
                                           maxTryCount - 1
                                             |> Just
                                             |> UpdateOptionLengths
                                             |> InitViewportsMsg
                              )
                   )

           InitViewportsMsg (SyncOptionLengths optionToLengthPairs) ->
                -- Important!
               -- update all the option lengths in the record
               -- and will update scene and viewport of `ForView` viewport
               -- and will update virtual viewport in turn
               --     if you follow the Cmd channel

               ( ( optionToLengthPairs
                     |> List.foldr
                        (\(optId, optLength) mbFoldedState ->
                             Maybe.map2
                             (\(OptionItem opt) foldedState ->
                                  foldedState
                                     |> replaceOption
                                        (wrapOption
                                             { opt |
                                               surfaceLength
                                                   = (Just optLength)
                                             }
                                        )
                             )
                             ( state.optionIdToItemDict
                                  |> Dict.get optId
                             )
                             mbFoldedState
                        )
                        (Just state)
                 )
                 |> (\mbStat ->
                         case mbStat of
                             Just state1 ->
                                 { state1 |
                                   optionLengthInfoStatus
                                       = OptionLengthUpdated
                                 , optionLengths
                                       = state1
                                       |> getOptions
                                       |> List.map .surfaceLength
                                       |> List.filterMap identity
                                 }
                             Nothing ->
                                 state
                                   |> resetPickerControl

                    )

               , ( Task.map
                       (InitViewportsMsg <<
                            Util.uncurry SyncViewportForView
                       )
                       taskGetViewViewportPosAndSceneLength

                   |> Task.onError
                      (always <| Task.succeed NoOp)
                   |> Task.perform messageMap
                 )
               )

           InitViewportsMsg (SyncViewportForView
                                   realViewportPos sceneLength) ->
               let state1
                       = { state |
                           lastSceneLength
                               = sceneLength
                         }
               in
                   ( state1
                     -- this involves virtual viewport change as well
                   , Task.map3
                         SyncVirtualControlPos
                         Time.now
                         (Task.succeed realViewportPos)
                         taskGetControlViewportPos

                   |> Task.onError
                         (always <| Task.succeed NoOp)
                      |> Task.perform messageMap
                   )

 
           OnScroll ->
               -- note: this Msg only getting from the `ForControl`
               --       not `'ForView`
               ( state

               , ( case state.optionLengthInfoStatus of
                       OptionLengthUpdated ->
                           Task.map3
                               SyncVirtualControlPos
                               Time.now
                               ( state.idString
                                    |> taskGetViewport
                                    |> Task.map
                                       ( .viewport >> posAccessor )
                               )
                               taskGetControlViewportPos
                           |> Task.onError
                                   (always <| Task.succeed NoOp)

                       _ ->
                           -- equivalent to initial state
                           InitViewportsMsg (UpdateOptionLengths Nothing)
                                |> Task.succeed


                 )
                 |> Task.perform messageMap
               )

           OnKey keyCodeString -> -- not used yet.
               let _ = Debug.log "key code:"
               in
                   ( state, Cmd.none )

           SyncVirtualControlPos clock
                                 lastRealViewportPosVal
                                 newControlViewportPosVal ->
               {- changes:
                  state: update `state.virtualControlPos` with
                         current virtual viewport position.
                         which involves syncing the state.virtualControlPos 
                         with real viewport position.

                  cmd:1. if virtual viewport position required to move,
                         `setViewport` as well. (optional)
                      2. send new Msg to sync new real viewport position

                  Virtual "Viewport" Pos ->
                  "Virtual Control Pos" -> (this is a helper data)
                  "Real"Viewport Pos ->
                -}

               case state.virtualControlPos
                       |> Virtual.getMaybeWarpInfo
                       |> Debug.log "warpInfo"
               of
                   Just _ ->
                       -- previously module-generated scroll
                       -- just update state.virtualControlPos
                       ( { state |
                           virtualControlPos
                               = ( clock, newControlViewportPosVal
                                             |> Virtual.makeViewportPos
                                 )
                               |> Virtual.initPos
                                  state.virtualControlSettings
                                  (pseudoState lastRealViewportPosVal)
                         }
                       , Cmd.none
                       )                       
                   _ ->
                       let
                           ( newVirtualControlPos, newRealViewportPos )
                               = state.virtualControlPos
                               |> Debug.log "origVirtualControlPos"
                               |> Virtual.updatePosAndRealViewportPos
                                  state.virtualControlSettings
                                  (pseudoState lastRealViewportPosVal
                                    |> Debug.log "withPseudoState")
                                  (clock
                                  , newControlViewportPosVal
                                      |> Virtual.makeViewportPos
                                  )
 
                           taskSyncLastScroll
                               = Time.now
                               |> Task.map
                                  (\now ->
                                       SyncLastScroll now
                                       newRealViewportPos
                                       ( ( state
                                             |> (not <<isPickerControlReady) )
                                         && ( state |> isSnapping )
                                       ) -- keepAnimation?
                                  )
                               |> Task.onError
                                  (always <| Task.succeed NoOp)

                       in
                           ( { state |
                               virtualControlPos
                                   = newVirtualControlPos
                                   |> Debug.log "newVirtualControlPos"
                             }
                           , ( taskSyncLastScroll ::
                                   case newVirtualControlPos
                                           |> Virtual.getMaybeWarpInfo
                                   of
                                       Just warpInfo ->
                                           SetViewport
                                           (SelectViewport ForControl)
                                           ( warpInfo.to
                                                |> Tuple.second
                                                |> Virtual.fromViewportPos
                                                |> toMilliPixel
                                           )
                                           |> Task.succeed
                                           |> List.singleton
                                       Nothing ->
                                           []
                             )
                             |> List.map
                                (Task.perform messageMap)
                             |> Cmd.batch
                           )

           SyncLastScroll clock destRealViewportPosVal keepAnimation ->
               -- sync from `ForControl` -> `ForView`
               -- if the `page` of control viewport needed to be changed,
               -- so be it.

               let
                    state1
                       = { state |
                           lastScrollClock
                               = clock
                         }

               in
                   if keepAnimation then
                       ( state1, Cmd.none )
                       -- don't bother to sync both viewports
                       -- during animation.
                       -- this happens only virtual viewport is not ready.
                   else
                       ( state1
                           |> stopSnapping
                           -- note: this is not immediate change
                           --       but it will after some time.
                           --       I guess that there is some issue with
                           --       accurate syncing

                       -- v .. and check the scrolling is stopped after
                       --      few milli seconds to try another snapping
                       , [ (Process.sleep << toFloat) state.scrollStopCheckTime
                              |> Task.andThen
                                 (always Time.now)
                              |> Task.perform (messageMap << TriggerSnapping)

                         , (SetViewport <| SelectViewport ForView)
                               (destRealViewportPosVal
                                    |> toMilliPixel)
                            |> Task.succeed
                            |> Task.perform messageMap
                         ]
                         |> Cmd.batch
                       )

           TriggerSnapping now ->
               if (state |> (not << isSnapping)) &&
                  (state |> isPickerControlReady) &&
                  (Time.posixToMillis
                       state.lastScrollClock + state.scrollStopCheckTime)
                                <= Time.posixToMillis now
               then
                   -- scroll has been stopped within `scrollStopCheckTime'
                   -- : start to snap to approriate option
                   ( state
                   , ( Task.map2
                       (\(frameCenterPos, _)
                            ( frameVpStartPos, frameVpCenterPos ) ->
                            FindCenterOptionWith
                            frameCenterPos
                            frameVpStartPos
                            frameVpCenterPos
                            SetSnapToTargetOption
                       )
                       taskGetControlCenterPosAndLength
                       (taskGetViewportPosAndCenterPos ForView)
                       -- ^ note: getting from `ForView` viewport
                     )
                     |> Task.attempt
                          (\res ->
                               messageMap <|
                               case res of
                                   Ok moduleMsg ->
                                       moduleMsg

                                   Err detailError ->
                                       ScrollPickerFailure
                                       "TriggerSnapping failed"
                                       ( "due to failure of getting dom"
                                         ++ "element of the frame"
                                       )
                                       detailError
                          )
                     )
               else
                   -- still animating or picker control is not ready
                   -- or another scroll happened within throttle timing.
                   ( state, Cmd.none )

           FindCenterOptionWith frameElCenterPos
                                frameVpStartPos frameVpCenterPos
                                onSuccess ->
               if state |> isPickerControlReady then
                   let
                       findCenterOptionHelper : Float -> List Float ->
                                                List String ->
                                                Maybe ( String, Float )
                                               -- middle position ^
                                               -- not start position

                       findCenterOptionHelper cursor lengthList optIdList
                           = case ( lengthList, optIdList ) of
                                 ( len :: restLengthList,
                                   oId :: restOptIdList ) ->
                                     let cursorCenter
                                             = (cursor + cursor + len)
                                               / 2
                                     in
                                         if ( frameVpCenterPos <= cursor+len )
                                             || ( restOptIdList
                                                    |> List.isEmpty )
                                     then
                                         Just ( oId
                                              , cursorCenter - frameVpCenterPos
                                              )

                                     else
                                         findCenterOptionHelper
                                             (cursor + len)
                                             restLengthList
                                             restOptIdList
                                 _ ->
                                     Nothing

                       findCenterOption
                           = findCenterOptionHelper
                             (paddingLength |> toFloat)
                             -- ^ padding at the beginning
                             state.optionLengths
                             state.optionIds

                   in
                       case findCenterOption of
                           Just (centerOptionIdString, optionRelPos) ->
                               ( state
                                   |> setCenterOption
                                      centerOptionIdString
                                      frameElCenterPos
                                      optionRelPos

                               , if  onSuccess "" -1 -1 -1
                                     == SyncCenterOption "" -1 -1 -1
                                     -- we cannot do
                                     -- `onSuccess == SyncCenterOption`
                                     -- : will crash
                                 then
                                         Cmd.none -- already done
                                 else
                                         Task.perform messageMap <|
                                         Task.succeed <|
                                         onSuccess
                                         centerOptionIdString
                                         frameElCenterPos
                                         frameVpStartPos
                                         optionRelPos
                               )

                           Nothing ->
                               ( state
                               , Task.perform messageMap <|
                                 Task.succeed <|
                                 ScrollPickerFailure
                                 "FindCenterOption"
                                 ( "this is a bug because when"
                                   ++ " isPickerControlReady is True,"
                                   ++ " always we could get one option"
                                 )
                                 ThisIsBug
                               )

               else
                   ( state
                   , Task.perform messageMap <|
                     Task.succeed <|
                     ScrollPickerFailure
                     ( state.idString ++ ":>"
                           ++ "FindCenterOptionWith" )
                     "could not get the option in the center"
                     PickerControllNotReady
                   )

           SyncCenterOption centerOptionIdString basePos _ relPos ->
               ( state
                   |> setCenterOption
                      centerOptionIdString basePos relPos
               , Cmd.none
               )

           SetSnapToTargetOption idString _ startPos relPos ->
               -- startPos is current viewport position
               -- relPos is relative amount of position to change

               let targetScrollPos
                       = startPos + relPos
                       |> cleanScrollPos
               in
                   -- set animation
                   ( { state |
                       targetIdString
                           = Just idString

                     , finalTargetScrollPosMP
                           = targetScrollPos
                           |> toMilliPixel

                     , pseudoAnimState
                           = initPseudoAnimState startPos
                           |> Animation.interrupt
                              [ Animation.to
                                    [  scrollPosProperty <| targetScrollPos
                                    ]
                              -- XXX how about animation duration ???
                              -- Change Animation.to -> Animation.toWith
                              -- or when making style using styleWith instead
                              ]
                     }

                   , Cmd.none
                   )

           GotoTargetOption optionIdString ->
               ( state
               , ( Task.map3
                       (\( frameCenterPos, _ ) frameVpPos
                            optionCenterPos ->
                            SetSnapToTargetOption
                            optionIdString
                            frameCenterPos -- not used but...
                            frameVpPos
                            (optionCenterPos - frameCenterPos)
                       )
                       taskGetControlCenterPosAndLength
                       taskGetControlViewportPos
                       (taskGetOptionCenterPos optionIdString)
                 |> Task.attempt
                    (\res ->
                         messageMap <|
                         case res of
                             Ok snapMsg ->
                                 snapMsg

                             Err detailError ->
                                 ScrollPickerFailure
                                 "GotoTargetOption"
                                 "could not go to the option"
                                 detailError
                    )
                 )
               )

           ScrollPickerSuccess option ->
               ( if state.targetIdString == (Just option.idString) then
                     state
                       |> stopSnapping
                 else
                     state
               , Cmd.none
               )

           ScrollPickerFailure context detailString detailError ->
               let _ = Debug.log context detailString
               in
                   -- handle error or log here
                   ( state
                   , Cmd.none
                   )

           Animate animMsg ->
               let
                   ( newAnimState, animCmd )
                       = Animation.Messenger.update
                         animMsg
                         state.pseudoAnimState

                   mbNewViewportPos
                       = if state |> isSnapping then
                             getMaybeAnimProperty
                             newAnimState scrollPosPropertyName
                                 |> Maybe.andThen String.toFloat
                         else
                             Nothing

               in
                   case mbNewViewportPos of
                       Just newViewportPos ->
                           let newViewportPosMP
                                   = newViewportPos
                                   |> toMilliPixel

                               scrollTraceMP
                                   = let (MilliPixel intVal)
                                             = newViewportPosMP
                                     in
                                         state.scrollTraceMP
                                           |> Set.insert intVal

                           in
                               ( { state |
                                   pseudoAnimState
                                       = newAnimState
                                 , scrollTraceMP
                                     = scrollTraceMP
                                     -- this value is not quite synchronized
                                     -- with 'OnScroll' Msg which catches the
                                     -- event later than one or two Animation
                                     -- happened already.
                                     -- So we will follow the trace of Animation
                                     -- position to check any 'scroll' events
                                     -- made from the this module in the end.
                                 }

                               , Cmd.batch
                                 [ ( if (state.finalTargetScrollPosMP
                                            |> isInScrollTraceHelper
                                            |> List.filter ((==) 0)
                                            |> List.length            ) > 1 then
                                         --^ found the same position more than
                                         --  once which probably mean
                                         --  the viewport already
                                         --  at the destination
                                         --  : this happends mainly because
                                         --  position  value in web browser is
                                         --  not `Float` but `Int`
                                         case state.targetIdString
                                                |> Maybe.andThen
                                                   (\idstr ->
                                                        state
                                                        |> .optionIdToItemDict
                                                        |> Dict.get idstr
                                                   )

                                         of
                                             Just (OptionItem targetOption) ->
                                                 Task.succeed <|
                                                     ScrollPickerSuccess
                                                     targetOption

                                             _ ->
                                                 Task.fail <|
                                                     InvalidOptionId
                                                     state.targetIdString
                                     else
                                         Task.succeed <|
                                             SetViewport (SelectViewport ForView)
                                             newViewportPosMP
                                    )
                                   |> Task.attempt
                                      (\res ->
                                           case res of
                                               Ok internalMsg ->
                                                   messageMap internalMsg
                                               _ ->
                                                   messageMap NoOp)

                                 , animCmd
                                 ]
                               )
                       _ ->
                           ( { state |
                               pseudoAnimState
                                   = newAnimState
                             }
                           , animCmd
                           )


           -- the Msg where acutally move the viewport
           SetViewport selectedViewport scrollPosMP ->
               let
                   taskSetViewport whichTarget
                       = state
                       |> taskSetViewportHelper appModel
                          whichTarget
                          (scrollPosMP |> fromMilliPixel)

                   targetList
                       = case selectedViewport of
                             SelectBothViewport ->
                                 [ ForView, ForControl ]

                             SelectViewport whichFor ->

                                 [ whichFor ]
               in
                   ( state
                   , targetList
                       |> List.map taskSetViewport
                       |> Task.sequence
                       |> Task.attempt
                          (\res ->
                               messageMap <|
                               case res of
                                   Ok _ ->
                                       NoOp

                                   Err detailError ->
                                       ScrollPickerFailure
                                       ( state.idString ++ ":>"
                                         ++" SetViewport failed")
                                       ( "could not animate due to"
                                         ++ "previous error"
                                       )
                                       detailError
                          )
                   )
           NoOp ->
               ( state, Cmd.none )


-- -- -- SUBSCRIPTIONS -- -- --

{-| this is how `subscriptionsWith` use `subscriptionsWithHelper`

```elm
subscriptionsWith pickerStates model
    = pickerStates
    |> subscriptionsWithHelper
       (\idString ->
            model.messageMapWith idString << Animate)
```
-}
subscriptionsWithHelper : (String -> Animation.Msg -> msg) ->
                          List { a |
                                 idString        : String
                               , pseudoAnimState : Animation.Messenger.State msg
                               } ->
                          Sub msg

subscriptionsWithHelper animMessageMapWith pickerStates
    = pickerStates
    |> List.map
       (\{idString, pseudoAnimState} ->
            Animation.subscription
            (animMessageMapWith idString)
            [ pseudoAnimState ]
       )
    |> Sub.batch

{-| Pass the list of the scroll states with your own application model
to inform the function `messageMapWith` function, and you will get
subscription (Sub msg).

**Important:** no animation will work without subscriptions!!!
-}
subscriptionsWith : List (BasicStateLike statExtra optExtra vt msg) ->
                    { model |
                      messageMapWith : (String -> (Msg optExtra vt msg) -> msg)
                    } ->
                    Sub msg

subscriptionsWith pickerStates model
    = pickerStates
    |> subscriptionsWithHelper
       (\idString ->
            model.messageMapWith idString << Animate)
