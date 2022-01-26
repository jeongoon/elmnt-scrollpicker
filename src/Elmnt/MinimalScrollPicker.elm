module Elmnt.MinimalScrollPicker
        exposing ( MinimalOption
                 , MinimalOptionLike
                 , OptionItem
                 --, OptionSubId
                 , Direction (..)
                 , StartEnd (..)
                 , MinimalState
                 , MinimalStateLike
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
                 , initMinimalState
                 , initCmdWith
                 , viewAsElement
                 , updateWith
                 , subscriptionsWith
                 , taskTriggerGetCenterOptionHelper

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
                 , getCenterPosOf
                 , taskGetViewport
                 , alwaysGotoOptionWithIdHelper
                 , toMilliPixel
                 , fromMilliPixel
                 , subscriptionsWithHelper
                 , viewAsElementHelper
              )


{-| This module is an implementation of picker by scrolling and basic view type is [`elm-ui`][elm-ui].
and animation can be done a bit tricky but easily thanks to [`elm-style-animation`][elm-style-animation].
Due to some non-standard way to hiding scrollbar, [`elm-css`][elm-css] is also required.

**Note:** Type annotation is probably too long to see. However, it might be useful if you
want add some feature with your own picker model.

[elm-ui]: /packages/mdgriffith/elm-ui/latest
[elm-css]: /packages/rtfeldman/elm-css/latest
[elm-style-animation]: /packages/mdgriffith/elm-style-animation/latest
[example]: https://github.com/jeongoon/elmnt-scrollpicker/tree/3.0.0/examples/ClockTeller.elm

# Type

@docs MinimalState, MinimalOption, OptionItem, Direction, StartEnd, Msg, Error

# State(picker model) Creation, Modification and Query

@docs initMinimalState, setOptions, getOptions,setScrollStopCheckTime,
anyNewOptionSelected,  initCmdWith

# Update

@docs updateWith

# Subscriptions

@docs subscriptionsWith

# View

@docs viewAsElement, defaultTheme, BaseTheme, BaseSettings

# Helper functions

@docs getOptionIdString, wrapOption, asOptionSubId

# Low-level Data types and functions

@docs MinimalStateLike, MinimalOptionLike, MinimalPaletteLike,
MinimalPaletteOnLike, getOptionsWrapped,
isSnapping, stopSnapping, hasCenterOption, setCenterOption, resetCenterOption,
unsafeSetScrollCheckTime, scrollPosProperty, scrollPosPropertyName,
initPseudoAnimState, initPseudoAnimStateHelper,
defaultShadeAttrsWith, defaultBaseSettingsWith, Geom, getCenterPosAndLengthOfHelper,
taskGetViewport, 
toMilliPixel, fromMilliPixel, subscriptionsWithHelper

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
-- ^ an example palette

{-| General prototype for option which has minimal set of fields
 -}
type alias MinimalOptionLike optionWith vt msg
    = { optionWith |
        idString        : String
      , index           : Int
      , value           : vt
      , element         : Element msg
      }


{-| Option record for each item in the list from which user will choose.

This record depends on the type of value and element (Element)
-}
type alias MinimalOption vt msg
    = MinimalOptionLike {} vt msg
                     -- ^ no extra option filed.


{-| A container for option types, which is useful when you try to
put some option that has type of superset of `MinimalOptionLike`
you can wrap it with [`wrapOption`](#wrapOption) unwrap with
[`unwrapOption`](#unwrapOption)

```elm
{ state |
  optionIdToItemDict
      = Dict.fromList
        [ wrapOption (YourOptionConstructor id index value element) ]
}
...
```
-}
type OptionItem extraOpt vt msg
    = OptionItem (MinimalOptionLike extraOpt vt msg)


{-| [`OptionItem`](#OptionItem) is an opaque type so you need wrapper and unwrapper
-}
wrapOption : MinimalOptionLike extraOpt vt msg ->
             OptionItem extraOpt vt msg
wrapOption option
    = OptionItem option


{-| unwrapper for [`OptionItem`](#OptionItem). this might be useuful only
when you are making derived version of this module.
-}
unwrapOption : OptionItem extraOpt vt msg -> MinimalOptionLike extraOpt vt msg
unwrapOption (OptionItem option)
    = option


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

type alias OnSuccessFunction extraOpt vt msg
    = String -> Float -> Float -> Float ->
      Msg extraOpt vt msg

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
type alias MinimalState extraOpt vt msg
    = MinimalStateLike {} extraOpt vt msg
                    -- ^ {} : no extra state field.


{-| Used for internal type checking -}
type alias MinimalStateLike extraStat extraOpt vt msg
    = { extraStat |
        idString                : String
      , optionIds               : List String
      , optionIdToItemDict      : Dict String (OptionItem extraOpt vt msg)
      , targetIdString          : Maybe String
      , pseudoAnimState         : Animation.Messenger.State msg
      -- ^ elm-style-animation doesn't support low-level functions
      --   so we need to make a pseudo `style' record

      , lastScrollClock         : Time.Posix
      , scrollTraceMP           : Set Int
      -- a trace of current animation set
      -- used for checking 'scroll' events coming from the module or user
      -- **Note:** Set is used because one direction Animation is in use.

      , finalTargetScrollPosMP  : Int           -- MP : Milli Pixels
      , scrollStopCheckTime     : Int
      , optionIdInTheCenter     : Maybe String
      , frameCenterPos          : Float
      , optionCenterRelPos      : Float
      }


{-| Msg chain generally covers the following steps

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
type Msg extraOpt vt msg
    = SyncLastScroll            Time.Posix Bool
    | OnScroll
    | OnKey                     String
    | FindCenterOption          (OnSuccessFunction extraOpt vt msg)
    | TriggerSnapping           Time.Posix
    | CheckInitialTargetOption  (OnSuccessFunction extraOpt vt msg)
                                -- (will be passed to DetermineTargetOption)
                                (List (OptionItem extraOpt vt msg))
                                -- ^ options before the sample
                                (OptionItem extraOpt vt msg)
                                -- ^ initial sample to check
                                (List (OptionItem extraOpt vt msg))
                                -- ^ options after the sample

    | DetermineTargetOption     (OnSuccessFunction extraOpt vt msg)
                                -- if the option nearest to the center of frame
                                -- what will we do next?
                                -- OnSuccessFunction used in this modules are ..
                                --   SetSnapToTargetOption
                                --   SyncCenterOption

                                (Result Error (List (OptionItem
                                                         extraOpt vt msg)
                                               --^  other candidates
                                              , Maybe ( String
                                                --^ current name of closest
                                                --  Option
                                                      , ( Float
                                                --^ frame position
                                                      , Float )
                                                      )
                                                --^ current closest position
                                                --  of an Option
                                              )
                                )

    | SyncCenterOption          String Float Float Float
                                --^ id, base, _  , rel pos  --  _ : not used
    | SetSnapToTargetOption     String Float Float Float
                                --^ id, _  , start, rel pos
    | GotoTargetOption          String
    | ScrollPickerSuccess       (MinimalOptionLike extraOpt vt msg)
    | ScrollPickerFailure       String String Error
    | Animate                   Animation.Msg
    | SetViewport               Int
    | NoOp


{-| Error used for Task _x_ a

**XXX:** this module doesn't really analyse the error status very well,
but those data types are explaining the stauts in the code instead of
any other types of comments.
-}
type Error
    = NoOptionAvailable
    | InvalidOptionId (Maybe String)
    | DomError Browser.Dom.Error
    | ScrollNegligible
    | CenterOptionUnavailable
    | ErrorMessageOnly
    | UnknownError


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

{-| Takes the direction of picker and gives the shade length
-}
defaultShadeLengthWith : Direction -> Int
defaultShadeLengthWith pickerDirection
    = case pickerDirection of
          Horizontal ->
              Util.sizeScaled 12
          Vertical ->
              Util.sizeScaled 8

{-| and helper function for shade elm-ui attributes (List Element.Attribute)
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
type alias BaseSettings compatible msg
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
      , cssWidthSetter          : Css.LengthOrAuto compatible -> Css.Style
      , cssOverFlowLongitudinal : Css.Overflow compatible -> Css.Style
      , cssOverFlowLateral      : Css.Overflow compatible -> Css.Style
      , fontSize                : Int
      , shadeLength             : Int
      , paddingLength           : Int
      , borderWidth             : Int
      , pickerLength            : Int
      , pickerWidth             : Int
      }


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
               ( (shadeLength |> toFloat) * 1.5 |> round )

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
                   |> truncate)

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
type alias StateUpdater extraStat extraOpt vt msg
    = MinimalStateLike extraStat extraOpt vt msg ->
      MinimalStateLike extraStat extraOpt vt msg

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
getOptionsWrapped : MinimalStateLike extraStat extraOpt vt msg ->
                    List (OptionItem extraOpt vt msg)
getOptionsWrapped { optionIds, optionIdToItemDict }
    =  optionIds
    |> List.map
       (Util.flip Dict.get <| optionIdToItemDict)
           |> List.filterMap identity


{-| get a list of option record data from the whole options by searching
option ID in a Dict.

The order of options in the same one of optionID list.
-}
getOptions : MinimalStateLike extraStat extraOpt vt msg ->
             List (MinimalOptionLike extraOpt vt msg)

getOptions state
    = getOptionsWrapped state
    |> List.map
       (\(OptionItem bare) -> bare)

{-| Save options from the list of pair of ( data, Element )
option Ids are stored separately and details stored in a Dict
there is no way to know how to make data value to string
you should suggest the function (vt -> String)

-}
setOptions : List ( OptionSubId, OptionItem extraOpt vt msg ) ->
             StateUpdater extraStat extraOpt vt msg

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
                               { option | -- update proper value
                                 idString
                                     = idString
                               , index
                                     = index
                               }
                         )
                    )
               )
            |> Dict.fromList

   in
       { state |
         optionIds
             = optionIds

       , optionIdToItemDict
             = optionIdToItemDict
       }


{-| check given option item is in the state.
-}
hasOption : OptionItem extraOpt vt msg ->
            MinimalStateLike extraStat extraOpt vt msg ->
            Bool
hasOption (OptionItem option) state
    = state.optionIds
    |> List.member option.idString
    
{-| replace option item if it exists in the state.

function for adding option(s) is not available. 
please use arrange your options
and use [`setOptions`](#setOptions) manually.
-}
replaceOption : OptionItem extraOpt vt msg ->
                StateUpdater extraStat extraOpt vt msg

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
                         { state |
                           scrollStopCheckTime  : Int
                         } ->
                         { state |
                           scrollStopCheckTime  : Int
                         }

setScrollStopCheckTime milliSeconds
    = unsafeSetScrollCheckTime (min 75 milliSeconds)


{-| You can test any value -- even under 75 ms
-- however which is not recommended
-}
unsafeSetScrollCheckTime : Int ->
                           { state |
                             scrollStopCheckTime : Int
                           } ->
                           { state |
                             scrollStopCheckTime : Int
                           }

unsafeSetScrollCheckTime milliSeconds state
    = { state |
        scrollStopCheckTime
            = milliSeconds
      }


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
anyNewOptionSelected : Msg extraOpt vt msg ->
                       Maybe (MinimalOptionLike extraOpt vt msg)
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
isSnapping : MinimalStateLike extraStat extraOptino vt msg ->
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
stopSnapping : StateUpdater extraStat extraOpt vt msg
stopSnapping state
    = { state |
        targetIdString
            = Nothing
      , finalTargetScrollPosMP
            = -1
      , scrollTraceMP
            = Set.empty
      , pseudoAnimState =
          initPseudoAnimState 0
      }

{-| check if we already know which item is is near to the center
-}
hasCenterOption : MinimalStateLike extraStat extraOpt vt msg ->
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
                  MinimalStateLike extraStat extraOpt vt msg ->
                  MinimalStateLike extraStat extraOpt vt msg
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
resetCenterOption : MinimalStateLike extraStat extraOpt vt msg ->
                    MinimalStateLike extraStat extraOpt vt msg
resetCenterOption state
    = { state |
        optionIdInTheCenter
            = Nothing
      }

-- -- -- Helper functions for Internal usage -- -- --

{-| [`Browser.Dom.Viewport`](/packages/elm/browser/latest/Browser-Dom#Viewport),
[`Browser.Dom.Element`](/packages/elm/browser/latest/Browser-Dom#Element)
share basic record accessor like `.x`  `.y`  `.width`  `.height`

getCenterPosOf function try to get center poisition of the some field.

ex) to get center 'y' position of viewport, you can try

```elm
aRecord
  |> getCenterPosOf .y .height .viewport
```
-}
getCenterPosOf : (Geom -> Float) -> (Geom -> Float) -> (rec -> Geom) -> rec -> Float
getCenterPosOf posAccessor lengthAccessor boxAccessor record
    = let
        box
            = record
            |> boxAccessor

        boxPos
            = box
            |> posAccessor

        boxLength
            = box
            |> lengthAccessor

   in
       boxPos + 0.5 * boxLength


{-| mbCurrentOption represents an option apears to be near the center of the
view frame and rest of them partitioned into two seprate list
-}
type alias OptionPartition extraOpt vt msg
    = { previousOptions : List  (OptionItem extraOpt vt msg)
      , mbCurrentOption : Maybe (OptionItem extraOpt vt msg)
      , nextOptions     : List  (OptionItem extraOpt vt msg)
      }

{-| when all the items are distributed uniformly, it might be easier to
get the option to focus(for snapping). partitionOptions will provide some
hints by offering (1) *one candidate* to snap and (2) *previous options*
 prior to current one and (3) *next options*.

Calulating relative position in Viewport probably be only way to test 
whether the target is correct one or not, so you should check the candidates
by checking relatiave position from the center of window frame.

**Note:** previous options are in reversed order, so first item is closest
to first sample and the last one is farthest.
-}

partitionOptionsHelper : (Geom -> Float) ->
                         (Geom -> Float) ->
                         MinimalStateLike extraStat extraOpt vt msg ->
                         { vp |
                           scene :
                               { d |
                                 height : Float
                               , width  : Float
                               }
                         , viewport : Geom
                         } ->
                         (OptionPartition extraOpt vt msg)


partitionOptionsHelper posAccessor lengthAccessor state viewport
    = let
        options
            = state
            |> getOptionsWrapped

        sceneLength
            = { x = -1
              , y = -1
              , width = viewport.scene.width
              , height = viewport.scene.height
              } -- to share same type annotation for lengthAccessor
                -- (scene itself doesn't have x, y fields)
            |> lengthAccessor

        windowCenterPos
            = viewport
            |> getCenterPosOf posAccessor lengthAccessor .viewport

        positionPercent
            = windowCenterPos / sceneLength

        lengthOfOpt
            = options
            |> List.length

        lengthOfOptWithPadding
            = lengthOfOpt + 2 -- count both padding as one element in terms of length

        truncateHelper offset x
            = x
            |> truncate
            |> clamp (0 + offset) (lengthOfOpt - 1 + offset)

        searchFrom
            = positionPercent * (lengthOfOptWithPadding
                                      |> toFloat)

            |> truncateHelper 0

        previousOptions
            = options
            |> List.take searchFrom

        restOptions
            = options
            |> List.drop searchFrom

        findFirstSample rp c n
           = let edgeCase rp_ c_ n_
                      = { previousOptions = rp_
                        , mbCurrentOption = c_
                        , nextOptions     = n_
                        }

              in
                  case ( rp, c ) of
                      ( _, Just option ) ->
                          edgeCase rp c n

                      ( [], Nothing ) ->
                          edgeCase rp c n

                      _ ->
                          -- if we drop too much and restOptions doesn't
                          -- have any, we will try another one from
                          -- previousOptions (if still available)
                          findFirstSample
                          (rp |> List.drop 1)
                          (rp |> List.head)
                          n
   in
       findFirstSample
           (previousOptions
                |> List.reverse) -- reversed for easier traversing
           (restOptions
                |> List.head)
           (restOptions
                |> List.drop 1)


{-| A Task helper function to get relative distance of the item from
frame which is measured from the center position of each other.
This value has sign -- negtative value shows that the item is
left or above the centre of view frame
-}
taskTargetOptionRelPosHelper : (Geom -> Float) ->
                               (Geom -> Float) ->
                               String ->
                               String ->
                               Task Error (Float, Float)

taskTargetOptionRelPosHelper posAccessor lengthAccessor
                             frameIdString optionItemIdString

    = ( (Browser.Dom.getElement frameIdString
             |> Task.mapError DomError
        )
        -- FIXME: frame is noramlly in the fixed position after page loading
        --        unless page is resized.
        --        : change routines update frame position occasionally

      , (Browser.Dom.getElement optionItemIdString
             |> Task.mapError DomError
        )
      )
    |> (Util.uncurry <|
            Task.map2
            (\frameDomElement optionDomElement ->
                 let basePos
                         = frameDomElement
                         |> getCenterPosOf posAccessor lengthAccessor .element
                     targetPos
                         = optionDomElement
                         |> getCenterPosOf posAccessor lengthAccessor .element

                 in
                     ( basePos
                     , targetPos - basePos
                     )
            )
       )


{-| Task helper to get viewport of the item id.
-}
taskGetViewport : String ->
                  Task Error Browser.Dom.Viewport

taskGetViewport idString
    = Browser.Dom.getViewportOf idString
    |> Task.mapError DomError

{-| Task helper to get element of the item id.
-}
taskGetElement : String ->
                 Task Error Browser.Dom.Element

taskGetElement idString
    = Browser.Dom.getElement idString
    |> Task.mapError DomError


{-| Task helper to retreive the position. posAccessor should find the 'x' or 'y'
position from the Geom data type.
-}
taskGetViewportPosHelper : (Geom -> Float) ->
                           String ->
                           Task Error Float

taskGetViewportPosHelper posAccessor idString
    = taskGetViewport idString
    |> Task.andThen
       (.viewport >> posAccessor >> Task.succeed)


{-| try to get the option item in the centre of a frame
and return a Task to attempt as following action
-}
taskTriggerGetCenterOptionHelper
    : (Geom -> Float) ->
      (Geom -> Float) ->
      MinimalStateLike extraStat extraOpt vt msg ->
      String ->
      (OnSuccessFunction extraOpt vt msg) ->
      Task Error (Msg extraOpt vt msg)

taskTriggerGetCenterOptionHelper posAccessor lengthAccessor state
                                 frameIdString onSuccess

    = taskGetViewport frameIdString
    |> Task.andThen
       (\vp ->
            let { previousOptions, mbCurrentOption,
                  nextOptions } = partitionOptionsHelper
                                  posAccessor lengthAccessor state vp
            in
                case mbCurrentOption of
                    Just firstSample ->
                        Task.succeed <|
                            CheckInitialTargetOption
                            onSuccess
                            previousOptions
                            firstSample
                            nextOptions

                    Nothing ->
                        -- which means no more previous option
                        -- only need to check next ones.
                        if List.isEmpty nextOptions then
                            Task.fail NoOptionAvailable
                        else
                            Task.succeed <|
                            DetermineTargetOption
                            onSuccess <|
                            Ok ( nextOptions, Nothing )
       )


{-| To set the target to select programatically,
you can call with Task.perform with `identity` function

```elm

let initModel
        = ...
in
   ( initModel
   , Task.perform identity
         ( yourAppModel
             |> MinimalScrollPicker.alwaysGotoOptionWithIdHelper initModel
                (MinimalScrollPicker.getOptionIdString
                     yourPickerState.idString
                     (MinimalScrollPicker.asOptionSubId optionSubIdString)
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
      MinimalStateLike statExtra optExtra vt msg ->
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

{-| An utility which converts an floating value to an integer value which
contains upto milli of base unit (pixel in this case)
-}
toMilliPixel : Float -> Int
toMilliPixel floatVal
    = floatVal * 1000
    |> truncate

{-| An utility which converts an integer value(which contains up to thousandth
value of original) to an float value.
-}
fromMilliPixel : Int -> Float
fromMilliPixel milliPixel
    = milliPixel
    |> toFloat
    |> (Util.flip (/)) 1000


-- -- -- INIT (Model only; no Cmd) -- -- --

{-| Helper function to initialise the minimal state(model). You can call
[`setOptions`](#setOptions) after this.

```elm
    initMinimalState "myPicker"
        |> setOptions
           String.fromInt
           [ ( 1, Element.text "1" )
           , ( 2, Element.text "2" )
           ...
```

-}
initMinimalState : String ->
                   MinimalState extraOpt vt msg

initMinimalState idString
    = { idString
            = idString
      , optionIds
            = []
      , optionIdToItemDict
            = Dict.empty
      , targetIdString
            = Nothing
      , pseudoAnimState
            = initPseudoAnimState 0
                -- ^ this is not actual Html Style elements
                --   just for storing some animation status
                --   `pos' is used for `Browser.Dom.setViewportOf'
      , lastScrollClock
            = Time.millisToPosix 0
      , scrollTraceMP
            = Set.empty
      , finalTargetScrollPosMP
            = -1
      , scrollStopCheckTime
            = 250 -- 250 ms
      , optionIdInTheCenter
            = Nothing
      , frameCenterPos
            = -1
      , optionCenterRelPos
            = -1
      }

{-| `initCmdWith` will make picker choose the initial option you speicify
with **sub** id string (not full id which you can access with .idString
from [`MinimalOptionLike`](#MinimalOptionLike) after [`setOptions`](#setOptions)

```elm
yourPickerState
  |> initCmdWith yourAppModel subOptionIdString
```
-}
initCmdWith : { appModelWith |
                messageMapWith : (String -> (Msg optExtra vt msg) -> msg)
              , pickerDirection : Direction
              } ->
              OptionSubId ->
              (MinimalStateLike statExtra optExtra vt msg) ->
              Cmd msg
initCmdWith ({ messageMapWith } as appModel) optionSubId state
    = [ state
          |> alwaysGotoOptionWithIdHelper appModel
             (getOptionIdString state.idString optionSubId)
             Nothing -- Maybe (error -> msg)
      ]
    |> List.map
       (Task.perform identity)
    |> Cmd.batch

-- -- -- View -- -- --

{-| Generating Element with theme setting and state value
each function only try to some state value in the whole record
so if you can apply this funciton with additional state you might want to use.

BaseTheme DOES NOT use all the color in the Palette. the Colors used
in the theme are 'accent', 'surface', 'background' 'on.background', 'on.surface'.
as you can see in the long signature

This means the color listed above are should be in your own palette at least,
even if you are using your own color accessor(function) with your theme.
-}
viewAsElement
    : { appModel |
        messageMapWith : (String -> (Msg extraOpt vt msg) -> msg)
      , pickerDirection : Direction
      } ->
      ( BaseThemeLike extraTheme (MinimalPaletteLike pal
                                  (MinimalPaletteOnLike palOn)) msg
      ) ->
      MinimalStateLike extraStat extraOpt vt msg ->
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
        messageMapWith : (String -> (Msg extraOpt vt msg) -> msg)
      , pickerDirection : Direction
      } ->
      ( BaseThemeLike extraTheme
            (MinimalPaletteLike pal
                 (MinimalPaletteOnLike palOn)) msg
      ) ->
      (Maybe (List (Element msg))) ->
      MinimalStateLike extraStat extraOpt vt msg ->
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
            = Html.Styled.div
              [ css [ Css.pseudoElement "-webkit-scrollbar"
                                        -- ^ no standard
                          [ Css.display Css.none ]

                    , Css.property "scrollbar-width" "none" -- standard
                    , cssWidthSetter <| Css.pct 100
                    , Css.border <| Css.px 5
                    , Css.borderColor
                          ( theme.borderColorFn
                               |>  getStyledColourWithDefault (.on >> .background)
                          )
                    , cssOverFlowLongitudinal Css.scroll
                    , cssOverFlowLateral Css.hidden
                    ]

              , Html.Styled.Attributes.id ( state.idString )
              , Html.Styled.Events.on "scroll" <|
                  Decode.succeed <| messageMap OnScroll
              ]

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
                      ( [ viewPickerPaddingElement ] ++
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
               messageMapWith : (String -> (Msg extraOpt vt msg) -> msg)
             , pickerDirection : Direction
             } ->
             (BaseThemeLike extraTheme
                  (MinimalPaletteLike palette
                       (MinimalPaletteOnLike paletteOn)) msg
             ) -> -- note: not used in MinimalScrollPicker
             Msg extraOpt vt msg ->
             MinimalStateLike extraStat extraOpt vt msg ->
             ( MinimalStateLike extraStat extraOpt vt msg
             , Cmd msg
             )

updateWith { messageMapWith, pickerDirection } _ msg state
    = let                                   -- ^ theme : not used here
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

        partitionOptions
            = partitionOptionsHelper posAccessor lengthAccessor

        taskTargetOptionRelPos
            = taskTargetOptionRelPosHelper posAccessor lengthAccessor

        taskGetViewportPos
            = taskGetViewportPosHelper posAccessor

        taskTriggerGetCenterOption
            = taskTriggerGetCenterOptionHelper posAccessor lengthAccessor state

        taskSetViewport idstr newPos
            = let
                setViewPortOf
                    = Browser.Dom.setViewportOf idstr
           in
               ( case pickerDirection of
                     Horizontal ->
                         setViewPortOf newPos 0
                     Vertical ->
                         setViewPortOf 0 newPos
               ) |> Task.mapError DomError

        isInScrollTraceHelper mp
            = {-(Debug.log "scroll trace:"-} state.scrollTraceMP{-)-}
            |> Set.toList
            |> List.map
               -- ^ make distance list
               (\mp1 ->
                    abs (mp1 - mp) // 1000
                        |> (*) 1000
               -- ^ truncate as a pixel value (not milli pixel)
               )

        isInScrollTrace mp -- milli pixel
            = isInScrollTraceHelper mp
            |> List.member 0
               -- ^ and find any distance is equal to zero.

        cleanScrollPos
            = truncate
              >> toFloat


   in
       case msg of
           OnScroll ->
               -- we are unable to distinguish between a scroll event whether
               -- made from user or programatically generated.
               --    (Browser.Dom.setViewport)
               -- we will check the viewport position against the any position
               -- animated during runtime.
               ( state
               , if state |> isSnapping then
                      taskGetViewport state.idString
                        |> Task.andThen
                           (\vp ->
                                let vpPos
                                        = vp.viewport |> posAccessor
                                    vpPosMP
                                        = vpPos |> toMilliPixel

                               in
                                   if vpPosMP |> isInScrollTrace then
                                       -- event from module => ignore
                                       Task.succeed True
                                   -- still animating ^
                                   else
                                       Task.succeed False
                                                 -- ^ do not keep animation
                           ) --^ result in 'keepAnimation'
                        |> Task.map2
                           Tuple.pair Time.now  -- i.e) (clock, keepAnimation)
                        |> Task.attempt
                           (\res ->
                                messageMap <|
                                    case res of
                                        Ok (clock, keepAnimation) ->
                                            SyncLastScroll clock keepAnimation

                                        Err detailError ->
                                            ScrollPickerFailure "OnScroll"
                                            "taskGetViewport maybe failed."
                                            detailError
                           )

                  else -- not even snapping: user scroll
                      Task.perform
                      (messageMap << (Util.flip SyncLastScroll) False)
                      Time.now
               )

           OnKey keyCodeString -> -- not used yet.
--               let _ = Debug.log "key code:"
--               in
                   ( state, Cmd.none )

           SyncLastScroll clock keepAnimation ->
               ( if keepAnimation then
                     ( state, Cmd.none )

                 else
                     ( state
                         |> stopSnapping

                     -- v .. and check the scrolling is stopped after
                     --      few milli seconds to try another snapping
                     , Process.sleep (toFloat state.scrollStopCheckTime)
                        |> Task.andThen
                           (always Time.now)
                        |> Task.perform (messageMap << TriggerSnapping)
                     )
               ) |> Tuple.mapFirst
                    (\m ->
                         { m | lastScrollClock = clock }
                    )

           TriggerSnapping now ->
               if state |> isSnapping then
                   ( state, Cmd.none )

               else
                   if (Time.posixToMillis
                           state.lastScrollClock + state.scrollStopCheckTime)
                       <= Time.posixToMillis now
                   then
                       -- scroll has been stopped within `scrollStopCheckTime'
                       -- : start to snap to approriate option

{- -- below cose is not working as I epxected
   -- because information from `SyncCenterOption` is probably outdated
   -- so just get fresh value to get snapping point.

                       if state |> hasCenterOption then
                           ( state
                           , Task.perform identity <|
                             Task.succeed <|
                             messageMap <|
                             case state.optionIdInTheCenter of
                                 Just optionId ->
                                     SetSnapToTargetOption
                                         optionId
                                         state.frameCenterPos
                                         state.optionCenterRelPos
                                 _ ->
                                     ScrollPickerFailure "TriggerSnapping"
                                         "center option supposed to be available but not."
                                         CenterOptionUnavailable
                           )
                       else
-}
                           ( state
                             -- or let's find out now and snap again
                           , Task.perform
                             (messageMap << FindCenterOption)
                             (Task.succeed SetSnapToTargetOption) -- onSuccess
                           )

                   else
                       -- another scroll happened within "Animation duration"
                       -- wait until new animation ready
                       ( state, Cmd.none )


           CheckInitialTargetOption onSuccess
                                    prevOpts
                                    (OptionItem currOpt)
                                    nextOpts ->
               ( state
               , taskTargetOptionRelPos state.idString currOpt.idString
                   |> Task.andThen
                      (\(basePos, relativePos) ->
                           Task.succeed
                           (if relativePos > 0.0 then
                                prevOpts -- check (to the left | upward)
                                         -- no meaning to check right hand side
                                         -- it will only go far way
                                         -- in the same direction
                            else
                                nextOpts -- check (to the right | downward)

                           , Just ( currOpt.idString, (basePos, relativePos) )
                           )
                      )
                   |> Task.attempt
                      ( messageMap << (DetermineTargetOption onSuccess) )
               )

           DetermineTargetOption onSuccess resCandidates ->
               ( state
               , case resCandidates of
                     Ok ( candidates, Nothing ) ->
                         -- no initial best candidates so far :
                         --   checking first sample from candidates
                         let
                             mbCandi = List.head candidates
                         in
                             case mbCandi of
                                 Just (OptionItem candi) ->
                                     taskTargetOptionRelPos
                                         state.idString
                                         candi.idString
                                     |> Task.andThen
                                        (\(basePos, relativePos) ->
                                             Task.succeed <|
                                             ( candidates
                                                |> List.drop 1
                                             , Just -- new sample for next time
                                                   ( candi.idString
                                                   , ( basePos
                                                     , relativePos
                                                     )
                                                   )
                                             )
                                        )
                                     |> Task.attempt
                                        (messageMap
                                             << DetermineTargetOption onSuccess)

                                 Nothing ->
                                     -- this is not possible case
                                     Cmd.none

                     Ok ( candidates, Just ( idString, (basePos, relPos) )) ->
                         let
                             mbCandi = List.head candidates

                             setTargetOption : (MinimalOptionLike
                                                    extraOpt vt msg) ->
                                               (Result Error
                                                    (Float, (Float, Float))) ->
                                               (Msg extraOpt vt msg)

                             setTargetOption candi
                                             resOfGetStartAndRelPos

                                 = case resOfGetStartAndRelPos of
                                       Ok (startPos, (basePos1, candiRelPos)) ->
                                           if abs candiRelPos <= abs relPos then
                                               -- compare to previous record
                                               -- and current candidate is
                                               -- closer to center but
                                               -- will check next one to decide
                                               DetermineTargetOption
                                                   onSuccess <|
                                                   Ok ( candidates
                                                        |> List.drop 1
                                               -- ^ Ok is needed because
                                               --   not using Task.attempt
                                               --   but using Task.perform
                                                      , Just
                                                            ( candi.idString
                                                            , ( basePos1
                                                              , candiRelPos
                                                              )
                                                            )
                                                      )
                                           else
                                               -- final decision!
                                               -- use previous one as a target
                                               onSuccess
                                                   idString
                                                   basePos startPos relPos

                                       Err detailError ->
                                            ScrollPickerFailure
                                            "DetermineTargetOption"
                                            "setTargetOption failed from previous error."
                                            detailError

                         in
                             case mbCandi of
                                 Just (OptionItem candi) ->
                                     taskTargetOptionRelPos
                                       state.idString candi.idString
                                         |> Task.map2
                                            Tuple.pair
                                            (state.idString
                                                |> taskGetViewportPos)

                                         |> Task.attempt
                                            (messageMap
                                                 << setTargetOption
                                                 candi
                                            )

                                 Nothing ->
                                     -- no more option
                                     -- use last one as target option
                                     ( state.idString |> taskGetViewportPos )
                                         |> Task.attempt
                                            (\res ->
                                                 messageMap <|
                                                 case res of
                                                     Ok startPos ->
                                                         onSuccess
                                                         idString
                                                         basePos
                                                         startPos relPos

                                                     Err detailError ->
                                                         ScrollPickerFailure
                                                         "DetermineTargetOption"
                                                         ( "no more option but "
                                                          ++"taskGetViewportPos"
                                                          ++"failed."
                                                         )
                                                         detailError
                                            )

                     Err detailError ->
                         Task.perform identity <|
                         Task.succeed <|
                         messageMap <|
                             ScrollPickerFailure
                             "DetermineTargetOption"
                             "failed to get candidates"
                             detailError

               )

           FindCenterOption onSuccess ->
               ( state
               , taskTriggerGetCenterOption
                   state.idString
                   onSuccess
                 |> Task.attempt
                    (\res ->
                         messageMap <|
                         case res of
                             Ok targetOptionMsg ->
                                 targetOptionMsg

                             Err detailError ->
                                ScrollPickerFailure
                                "FindCenterOption: "
                                "taskTrggerGetCenterOption failed."
                                detailError
                    )
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
               ( { state |
                   targetIdString
                       = Just optionIdString
                 }
               , ( taskGetViewportPos state.idString
                 , taskTargetOptionRelPos state.idString optionIdString )
                   |> (Util.uncurry <|
                           Task.map2 Tuple.pair
                      )  -- ^ takes two tasks above and make a pair
                   |> Task.attempt
                      (\res ->
                           messageMap <|
                           case res of
                               Ok ( startPos, (basePos, relPos) ) ->
                                   SetSnapToTargetOption
                                   optionIdString basePos startPos relPos
                               _ ->
                                   NoOp
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
--               let _ = Debug.log context detailString
--               in
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
                                   = state.scrollTraceMP
                                   |> Set.insert newViewportPosMP

                           in
                               ( { state |
                                   pseudoAnimState
                                       = newAnimState
                                 , scrollTraceMP
                                     = scrollTraceMP
                                     -- this value is not quite synchronize
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
                                     --  the viewport already at the destination
                                         case state.targetIdString
                                                |> Maybe.andThen
                                                   ((Util.flip Dict.get)
                                                    state.optionIdToItemDict)
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
                                             SetViewport
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
           SetViewport scrollPosMP -> -- MP : in Milli Pixel
               ( state
               , taskSetViewport state.idString
                   (scrollPosMP |> fromMilliPixel)
                        |> Task.attempt
                           (always <| messageMap NoOp)
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
subscriptionsWith : List (MinimalStateLike extraStat extraOpt vt msg) ->
                    { model |
                      messageMapWith : (String -> (Msg extraOpt vt msg) -> msg)
                    } ->
                    Sub msg

subscriptionsWith pickerStates model
    = pickerStates
    |> subscriptionsWithHelper
       (\idString ->
            model.messageMapWith idString << Animate)
