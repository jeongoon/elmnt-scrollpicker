module Elmnt.BaseScrollPicker 
        exposing ( Option
                 , Direction (..)
                 , StartEnd (..)
                 , MinimalState
                 , Msg (..)
                 , Error
                 , BaseTheme
                 , defaultTheme
                 --, defaultFontSize
                 --, defaultBorderWidth
                 , BaseSettings
                 --, scrollPosPropertyName
                 --, scrollPosProperty
                 --, initPseudoAnimStateHelper
                 --, initPseudoAnimState
                 , getOptions
                 , setOptions
                 , setScrollStopCheckTime
                 , unsafeSetScrollCheckTime
                 , getOptionIdString
                 , anyNewOptionSelected
                 , initMinimalState
                 , viewAsElement
                 , updateWith
                 , subscriptionsWith

                 -- v  low-level api
                 , isSnapping
                 , stopSnapping
                 , defaultShadeLengthWith
                 , defaultShadeAttrsWith
                 , defaultBaseSettingsWith
                 , Geom
                 , getCenterPosOf
                 , partitionOptionsHelper
                 , getRelPosOfElement
                 , taskTargetOptionRelPosHelper
                 , taskGetViewport
                 , taskGetViewportPosHelper
                 , toMilliPixel
                 , fromMilliPixel
                 , subscriptionsWithHelper
               )

                    

{-| This module is an implementation of picker by scrolling and basic view type is [`elm-ui`][elm-ui].
and animation can be done a bit tricky but easily thanks to [`elm-style-animation`][elm-style-animation].
Due to some non-standard way to hiding scrollbar, [`elm-css`][elm-css] is also required.

**Note:** Type annotation is probably too long to see. However, it might be useful if you
want add some feature with your own picker model.

[elm-ui]: /packages/mdgriffith/elm-ui/latest
[elm-css]: /packages/rtfeldman/elm-css/latest
[elm-style-animation]: /packages/mdgriffith/elm-style-animation/latest
[exampleUpdate]: /packages/jeongoon/elmnt-scrollpicker/latest/#Update

# Type

@docs MinimalState, Direction, StartEnd, Option, Msg, Error

# **State(picker model) Creation, Modification and Query**

@docs initMinimalState, setOptions, getOptions, setScrollStopCheckTime, anyNewOptionSelected

# Update

@docs updateWith

# Subscriptions

@docs subscriptionsWith

# View
 
@docs viewAsElement, defaultTheme, BaseTheme, BaseSettings

# Helper functions

@docs getOptionIdString

# Low-level Data types and functions

@docs isSnapping, stopSnapping, unsafeSetScrollCheckTime, defaultShadeLengthWith,
defaultShadeAttrsWith, defaultBaseSettingsWith, Geom, getCenterPosOf,
partitionOptionsHelper, getRelPosOfElement, taskTargetOptionRelPosHelper,
taskGetViewport, taskGetViewportPosHelper, toMilliPixel, fromMilliPixel,
subscriptionsWithHelper

# FIXME

    - add keyboard input support

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
import Html.Attributes          as HtmlAttr
import Html.Events              as HtmlEvents

import Json.Encode              as Encode
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


{-| Option record for each item in the list from which user will choose.

This record depends on the type of value and element (Element)
-}
type alias Option vt msg
    = { idString        : String
      , index           : Int
      , value           : vt
      , element         : Element msg
      }
    

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
type alias MinimalState vt msg
    = { idString                : String
      , optionIds               : List String
      , optionIdToRecordDict    : Dict String (Option vt msg)
      , targetIdString          : Maybe String
      , pseudoAnimState         : Animation.Messenger.State msg
      -- ^ elm-style-animation doesn't support low-level functions
      --   so we need to make a pseudo `style' record

      , lastScrollClock         : Time.Posix
      , scrollTraceMP           : Set Int
      -- a trace of current animation set
      -- used for checking 'scroll' events coming from the module or user
      -- **Note:** Set is used because one direction Animation is in use.

      , finalTargetScrollPosMP  : Int           -- MP : Milli Seconds
      , scrollStopCheckTime     : Int
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

```elm
type Msg vt msg
    = SyncLastScroll            Time.Posix Bool
    | OnScroll
    | TriggerSnapping           Time.Posix
    | CheckInitialTargetOption
          (List (Option vt msg))
          -- ^ options before the sample
          (Option vt msg)
          -- ^ initial sample to check
          (List (Option vt msg))
          -- ^ options after the sample

    | DetermineTargetOption
          (Result Error (List (Option vt msg)
                              --^  other candidates
                        , Maybe ( String
                                  --^ current name of closest
                                  --  Option
                                 , Float )
                                 --^ current closest position
                                 --   of an Option
                        )
          )
          

    | SetSnapToTargetOption     String Float Float
                                -- ^ id, frame position,
                                --   relative pos to snap
    | MoveToTargetOption        String
    | ScrollPickerSuccess       (Option vt msg)
    | ScrollPickerFailure       Error
    | Animate                   Animation.Msg
    | AnimateSnapping           Int
    | NoOp
```
-}
type Msg vt msg
    = SyncLastScroll            Time.Posix Bool
    | OnScroll
    | OnKey                     String
    | TriggerSnapping           Time.Posix
    | CheckInitialTargetOption  (List (Option vt msg))
                                -- ^ options before the sample
                                (Option vt msg)
                                -- ^ initial sample to check
                                (List (Option vt msg))
                                -- ^ options after the sample

    | DetermineTargetOption     (Result Error (List (Option vt msg)
                                               --^  other candidates
                                              , Maybe ( String
                                                --^ current name of closest
                                                --  Option
                                                      , Float )
                                                --^ current closest position
                                                --  of an Option
                                              )
                                )

    | SetSnapToTargetOption     String Float Float  --> id and relative position
    | MoveToTargetOption        String
    | ScrollPickerSuccess       (Option vt msg)
    | ScrollPickerFailure       Error
    | Animate                   Animation.Msg
    | AnimateSnapping           Int
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
    | AnotherScrollBlockSnapping
    | ScrollNegligible
    

{-| An example settings value type in use here
-}
type alias BaseTheme palette msg
    = { palette           : palette
      , borderWidth       : Theme.Value Int
      , borderColorFn     : Theme.Value (palette -> Color)
      , shadingColorFn    : Theme.Value (palette -> Color)
      , focusColorFn      : Theme.Value (palette -> Color)
      , backgroundColorFn : Theme.Value (palette -> Color)
      , fontColorFn       : Theme.Value (palette -> Color)
      , fontSize          : Theme.Value Int
      , shadeLength       : Theme.Value Int
      , pickerLength      : Theme.Value Int
      , pickerWidth       : Theme.Value Int
      , shadeAttrsFn      : Theme.Value (Direction -> StartEnd -> List (Attribute msg))

      }


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
          BaseTheme
          --                 1  2  3  4  5  6  7  8  9 10 11 12
          Theme.defaultPalette df df df df df df df df df df df


{-|
-}
defaultFontSize : Int
defaultFontSize
    = Util.sizeScaled 8

{-|
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
defaultShadeAttrsWith : (BaseTheme
                             { palette |
                               accent : Color
                             , surface : Color
                             , background : Color
                             , on : { paletteOn |
                                      background : Color
                                    , surface    : Color
                                    }
                             , toElmUiColor : Color -> Element.Color
                             }
                             msg
                        ) ->
                        Direction ->
                        StartEnd ->
                        List (Attribute msg)

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
           = theme
            |> defaultBaseSettingsWith pickerDirection

        pickerWidthSetter
            = (px pickerWidth) |> minimum 15 |> widthSetter

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
                          |> Theme.withDefault ( .on >> .surface )
                       )
                    |> theme.palette.toElmUiColor
              )
          , MAttr.style attachedTo "0px"
          , MAttr.style "background-image" <|
              backgroundImageStyle ("to " ++ gradientTo)
                  <| ( theme.palette
                        |> (  theme.shadingColorFn
                                |> Theme.withDefault ( .on >> .surface )
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
      , borderWidth             : Int
      , pickerLength            : Int
      , pickerWidth             : Int
      }
                                          

{-| Generate setting values for a picker which has `Direction`
-}
defaultBaseSettingsWith : Direction ->
                      { theme |
                        fontSize     : Theme.Value Int
                      , borderWidth  : Theme.Value Int
                      , shadeLength  : Theme.Value Int
                      , pickerLength : Theme.Value Int
                      , pickerWidth  : Theme.Value Int
                      } ->
                      BaseSettings compatible msg

defaultBaseSettingsWith pickerDirection theme
    = let
        borderWidth
            = theme.borderWidth
            |> Theme.withDefault defaultBorderWidth

        shadeLength
            = theme.shadeLength
            |> Theme.withDefault
               ( defaultShadeLengthWith pickerDirection )

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
       fontSize shadeLength borderWidth pickerLength pickerWidth
                          

{-| property name used for Animation which will be sent to 
"Browser.Dom.setViewportOf' eventually
-}
scrollPosPropertyName : String
scrollPosPropertyName = "spos"

{-| function to make actuall property for scroll animation
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


-- -- -- Helper functions for user -- -- --

{-| get a list of Option record data from the whole options by searching
option ID in a Dict.

The order of options in the same one of optionID list.
-}
getOptions : { state |
               optionIds : List String
             , optionIdToRecordDict : Dict String (Option vt msg)
             } -> List (Option vt msg)

getOptions { optionIds, optionIdToRecordDict }
    = optionIds
    |> List.map
       (Util.flip Dict.get <| optionIdToRecordDict)

    |> List.filterMap identity


{-| Save options from the list of pair of ( data, Element )
option Ids are stored separately and details stored in a Dict
there is no way to know how to make data value to string
you should suggest the function (vt -> String)

-}
setOptions : (vt -> String) ->
             List (vt, Element msg) ->
             { state |
               idString  : String
             , optionIds : List String
             , optionIdToRecordDict : Dict String (Option vt msg)
             } ->
             { state |
               idString  : String
             , optionIds : List String
             , optionIdToRecordDict : Dict String (Option vt msg)
             }

setOptions toStringFn valueToElementPairs state
    = let
        optionIds
            = valueToElementPairs
            |> List.map
               (\(val, _) ->
                    getOptionIdString toStringFn state.idString val
               )

        indexedOptionIds
            = optionIds
            |> List.indexedMap Tuple.pair

        optionIdToRecordDict
            = (indexedOptionIds, valueToElementPairs)
            |> (Util.uncurry <| List.map2
                    (\(index, idString) (val, element) ->
                         ( idString, Option idString index val element )
                    )
               )
            |> Dict.fromList
              
   in
       { state |
         optionIds
             = optionIds

       , optionIdToRecordDict
             = optionIdToRecordDict
       }

{-| Every scroll is being watched to check whether it is stopped at the moment
and this function will change the timing to wait until checking.

**Limitation:** minimum value is 75 (ms). Animation will fail or work unexpectedly
under 75 ms.
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


{-| You can test any value -- even under 75 ms -- however which is not recommended
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


{-| make option id string value for 'Option.idString' which will be
useful if you want to access the id on the page.
-}
getOptionIdString : (vt -> String) ->
                    String ->
                    vt ->
                    String
getOptionIdString toStringFn pickerIdString optionValue
    = let
        concator
            = "~"
   in
       pickerIdString ++ concator ++ (optionValue |> toStringFn)

    
{-| Check the Msg, and return if there is any new selected option

please check this [Example][exampleUpdate].
-}
anyNewOptionSelected : Msg vt msg -> Maybe (Option vt msg)
anyNewOptionSelected msg
    =
      case msg of
          ScrollPickerSuccess option ->
              Just option
          _ ->
              Nothing

{-|
minimal testing function if the picker is snapping to some item
at the moment
-}
isSnapping : { state | targetIdString : Maybe String } -> Bool
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
stopSnapping : { state |
                 targetIdString         : Maybe String
               , finalTargetScrollPosMP : Int
               , scrollTraceMP          : Set Int
               , pseudoAnimState        : Animation.Messenger.State msg
               } ->
               { state |
                 targetIdString         : Maybe String
               , finalTargetScrollPosMP : Int
               , scrollTraceMP          : Set Int
               , pseudoAnimState        : Animation.Messenger.State msg
               }

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


-- -- -- Helper functions for Internal usage -- -- --

{-| [`Browser.Dom.Viewport`](/packages/elm/browser/latest/Browser-Dom#Viewport), [`Browser.Dom.Element`](/packages/elm/browser/latest/Browser-Dom#Element) share basic record accessor
like `.x`  `.y`  `.width`  `.height`

getCenterPosOf function try to get center poisition of the some field.

ex) to get center 'y' position of viewport, you can try

```elm
getCenterPosOf .y .height .viewport aRecord
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
type alias OptionPartition vt msg
    = { previousOptions : List  (Option vt msg)
      , mbCurrentOption : Maybe (Option vt msg)
      , nextOptions     : List  (Option vt msg)
      }

{-| when all the items are distributed uniformly, it might be easier to
get the option to focus(for snapping). partitionOptions will provide one
candidate to snap and previous options prior to current one and next
options as well.
Calulating relative position in Viewport probably be only way to test 
whether the target is correct one or not, however you might need to check 
around current candidate.
-}
partitionOptionsHelper : ( Geom -> Float ) ->
                         ( Geom -> Float ) ->
                         { state |
                           optionIdToRecordDict :
                               Dict.Dict String (Option vt msg)
                         , optionIds : List String
                         } ->
                         { vp |
                           scene :
                               { d |
                                 height : Float
                               , width  : Float
                               }
                         , viewport : Geom
                         } ->
                         (OptionPartition vt msg)

partitionOptionsHelper posAccessor lengthAccessor state viewport
    = let
        options
            = getOptions state

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
                        , mbCurrentOption   = c_
                        , nextOptions     = n_
                        }

              in
                  case ( rp, c ) of
                      ( _, Just option ) ->
                          edgeCase rp c n

                      ( [], Nothing ) ->
                          edgeCase rp c n

                      _ ->
                          -- if we drop too much and restOptions doesn't have any
                          -- we will try another one from previousOptions (if still available)
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


{-| To get relative position of element in the viewport.

you need to apply position accessor and length accessor which are normally
`.x` and `.width` for Horizontal scroll picker and `.y` and `.height` for
Vertical scroll picker.

**Note:** the element is got from getElement, viewport is got from getViewport
-}
getRelPosOfElement : (Geom -> Float) ->
                     (Geom -> Float) ->
                     { pos | element : Geom, viewport : Geom } -> Float

getRelPosOfElement posAccessor lengthAccessor pos
    = (pos
        |> getCenterPosOf posAccessor lengthAccessor .element)
      -
      (pos
        |> getCenterPosOf posAccessor lengthAccessor .viewport)


{-| A Task helper function to get relative distance of the item from
frame which is measured from the center position of each other.
This value has sign -- negtative value shows that the item is
left or above the centre of view frame
-}
taskTargetOptionRelPosHelper : (Geom -> Float) ->
                               (Geom -> Float) ->
                               String ->
                               String ->
                               Task Error Float

taskTargetOptionRelPosHelper posAccessor lengthAccessor
                             frameIdString optionItemIdString

    = ( (Browser.Dom.getElement frameIdString
             |> Task.mapError DomError
        )
      , (Browser.Dom.getElement optionItemIdString
             |> Task.mapError DomError
        )
      )
    |> (Util.uncurry <|
            Task.map2
            (\frameDomElement optionDomElement ->
                 let pos
                         = { viewport = frameDomElement.element
                           , element  = optionDomElement.element
                           }
                 in
                     getRelPosOfElement posAccessor lengthAccessor pos
            )
       )


{-| Task helper to get viewport of the item id.
-}
taskGetViewport : String ->
                  Task Error Browser.Dom.Viewport

taskGetViewport idString
    = Browser.Dom.getViewportOf idString
    |> Task.mapError DomError


{-| Task helper to retreive the position. posAccessor
-}
taskGetViewportPosHelper : ( Geom -> Float ) ->
                           String ->
                           Task Error Float

taskGetViewportPosHelper posAccessor idString
    = taskGetViewport idString
    |> Task.andThen
       (.viewport >> posAccessor >> Task.succeed)


{-| An utility which converts an floating value to an integer value which contains
upto milli of base unit (pixel in this case)
-}
toMilliPixel : Float -> Int
toMilliPixel floatVal
    = floatVal * 1000
    |> truncate

{-| An utility which converts an integer value(which contains up to thousandth value
of original) to an float value.
-}
fromMilliPixel : Int -> Float
fromMilliPixel milliPixel
    = milliPixel
    |> toFloat
    |> (Util.flip (/)) 1000


-- -- -- INIT (Model only; no Cmd) -- -- --

{-| Helper function to initialise the minimal model. You can call 
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
                   MinimalState vt msg

initMinimalState idString
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
      }

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
viewAsElement : { appModel |
                  messageMapWith : (String -> (Msg vt msg) -> msg)
                , pickerDirection : Direction
                } ->
                ( BaseTheme { palette |
                              -- ^ minimal palette to work with.
                              --   you should have even if don't use with
                              --   your own 'BaseTheme' settings.
                              accent : Color
                            , surface : Color
                            , background : Color
                            , on : { paletteOn |
                                     background : Color
                                   , surface    : Color
                                   }
                            , toElmUiColor : Color -> Element.Color
                            }
                      msg
                ) ->
                { state |
                  idString   : String
                , optionIds  : List String
                , optionIdToRecordDict : Dict String (Option vt msg)
                } ->
                Element msg

viewAsElement { messageMapWith, pickerDirection } theme state
    = let
        messageMap
            = messageMapWith state.idString

        { lengthSetter, widthSetter, longitudinalContainer,
          ancherString, windowEdges, centerLateral, cssWidthSetter,
          cssOverFlowLongitudinal, cssOverFlowLateral,
          fontSize, borderWidth, shadeLength, pickerWidth, pickerLength } =

            theme
                |> defaultBaseSettingsWith pickerDirection


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
                  |> Theme.withDefault (defaultShadeAttrsWith theme)
              )
              pickerDirection startEnd
 
        
        viewPickerPaddingElement
            = el [ (shadeLength |> toFloat) * 1.5 |> round |> px |> lengthSetter
                  , fill |> widthSetter
                 ] <| none
              -- ^ need some padding to centerize first and last item
              --   (or we can use padding ???

        viewPicker
            = Html.Styled.div
              [ css [ Css.pseudoElement "-webkit-scrollbar" [ Css.display Css.none ]
                    -- no standard
                    , Css.property "scrollbar-width" "none" -- standard
                    , cssWidthSetter <| Css.pct 100
                    , Css.border <| Css.px 5
                    , Css.borderColor <|
                        getStyledColourWithDefault (.on >> .background) theme.borderColorFn
                    , cssOverFlowLongitudinal Css.scroll
                    , cssOverFlowLateral Css.hidden
                    ]

              , Html.Styled.Attributes.id state.idString
              , Html.Styled.Events.on "scroll" <|
                  Decode.succeed <| messageMap OnScroll
              ]

              -- this isn't consistent solution but I don't want to think more about css
              -- so I'm going back to elm-ui.
              [ ( layoutWith { options = [ Element.noStaticStyleSheet ] }
                      [] <|

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
                      ( List.concat
                            [ [ viewPickerPaddingElement ]
 
                            , getOptions state
                                |> List.map
                                   (\opt ->
                                        el [ opt.idString |> MAttr.id 
                                           , pickerLength - shadeLength * 2 |> px |> lengthSetter
                                           , centerLateral
                                           ] <|
                                        el [ centerX
                                           , centerY
                                           ] opt.element
                                   )

                            , [ viewPickerPaddingElement ]

                            ]
                      )
                ) |> Html.Styled.fromUnstyled
              ]
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
         -- v window to see the selected item
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
                , (pickerLength - (shadeLength - borderWidth) * 2) |> px |> lengthSetter
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
updateWith : { a |
               messageMapWith : (String -> (Msg vt msg) -> msg)
             , pickerDirection : Direction
             } ->
             Msg vt msg ->
             { b |
               idString                 : String
             , lastScrollClock          : Time.Posix
             , scrollTraceMP            : Set Int
             , finalTargetScrollPosMP   : Int
             , scrollStopCheckTime      : Int
             , optionIdToRecordDict     : Dict String (Option vt msg)
             , optionIds                : List String
             , pseudoAnimState          : Animation.Messenger.State msg
             , targetIdString           : Maybe String
             } ->
             ( { b |
                 idString                  : String
               , lastScrollClock           : Time.Posix
               , scrollTraceMP            : Set Int
               , finalTargetScrollPosMP   : Int
               , scrollStopCheckTime       : Int
               , optionIdToRecordDict      : Dict String (Option vt msg)
               , optionIds                 : List String
               , pseudoAnimState           : Animation.Messenger.State msg
               , targetIdString            :   Maybe String
               }
             , Cmd msg
             )
       
updateWith { messageMapWith, pickerDirection } msg state
    = let
        messageMap
            = messageMapWith state.idString

        getMaybeAnimProperty animState propName
            -- `elm-style-animation' doesn't seem to supply the low level accessor
            -- and pseudoAnimState has only one member so...
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


   in case msg of
           OnScroll ->
               -- we are unable to distinguish between a scroll event whether
               -- user made or another which Browser.Dom.setViewport generated,
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
                                        Task.succeed (Debug.log "from module" True)
                                                          -- still anmatiting ^
                                    else
                                        Debug.log "from user" <|
                                        Task.succeed False
                                                  -- ^ do not keep animation
                           )
                        |> Task.map2
                           Tuple.pair Time.now  -- i.e) (clock, keepAnimation)
                        |> Task.attempt
                           (\res ->
                                case res of
                                    Ok (clock, keepAnimation) ->
                                        messageMap <| SyncLastScroll clock keepAnimation
                                    _ ->
                                        messageMap <| NoOp
                           )

                 else
                     Task.perform
                     (messageMap << (Util.flip SyncLastScroll) False)
                     (Debug.log "user scroll:" Time.now)
               )

           OnKey keyCodeString ->
               let _ = Debug.log "key code:"
               in
                   ( state, Cmd.none )

           SyncLastScroll clock keepAnimation ->
               ( if keepAnimation then
                     ( state, Cmd.none )

                 else
                     ( state |> stopSnapping
                     -- v and check the scrolling is stopped after few milli seconds
                     -- to try another snapping
                     , Process.sleep (toFloat state.scrollStopCheckTime)
                        |> Task.andThen
                           (always Time.now)
                        |> Task.perform (messageMap << TriggerSnapping)
                     )
               ) |> Tuple.mapFirst
                    ( \m -> { m | lastScrollClock = clock } )


           TriggerSnapping now ->
               if state |> isSnapping then
                   ( state, Cmd.none )

               else
                   if (Time.posixToMillis
                           state.lastScrollClock + state.scrollStopCheckTime)
                       <= Time.posixToMillis now
                   then
                       -- scroll is stopped at least during `scrollStopCheckTime'
                       -- : start to snap to approriate option

                       ( state
                       , taskGetViewport state.idString
                           |> Task.andThen
                              ( \vp ->
                                    let { previousOptions, mbCurrentOption, nextOptions } = partitionOptions state vp
                                    in
                                        case mbCurrentOption of
                                            Just firstSample ->
                                                Task.succeed <|
                                                CheckInitialTargetOption
                                                previousOptions
                                                firstSample
                                                nextOptions

                                            Nothing ->
                                                if List.isEmpty nextOptions then
                                                    Task.fail NoOptionAvailable
                                                else
                                                    Task.succeed <|
                                                    DetermineTargetOption <|
                                                    Ok ( nextOptions, Nothing )
                              )
                           |> Task.attempt
                              (\res ->
                                   case res of
                                       Ok targetOptionMsg ->
                                           messageMap targetOptionMsg
                                       Err x ->
                                           let _ = Debug.log "error:" x
                                           in
                                               messageMap NoOp
                              )
                       )
                   else
                       -- another scroll happened within "Animation duration"
                       -- wait until new animation ready
                       ( state, Cmd.none )


           CheckInitialTargetOption prevOpts currOpt nextOpts ->
               ( state
               , taskTargetOptionRelPos state.idString currOpt.idString
                   |> Task.andThen
                      (\relativePos ->
                           Task.succeed
                           (if relativePos > 0.0 then
                                prevOpts -- check (to the left | upward)
                                         -- no meaning to check right hand side
                                         -- it will only increase the relative poisition
                                         -- in the same direction
                            else
                                nextOpts -- check (to the right | downward)
                           , Just ( currOpt.idString, relativePos )
                           )
                      )
                   |> Task.attempt
                      (messageMap << DetermineTargetOption)
               )
                   
           DetermineTargetOption resCandidates ->
               ( state
               , case resCandidates of
                     Ok ( candidates, Nothing ) ->
                         -- checking first sample from candidates
                         let
                             mbCandi = List.head candidates
                         in
                             case mbCandi of
                                 Just candi ->
                                     taskTargetOptionRelPos
                                         state.idString
                                         candi.idString
                                             |> Task.andThen
                                                (\relativePos ->
                                                     Task.succeed <|
                                                     ( candidates |> List.drop 1
                                                     , Just
                                                         ( candi.idString
                                                         , relativePos
                                                         )
                                                     )
                                                )
                                             |> Task.attempt
                                                (Debug.log "test" << messageMap << DetermineTargetOption)

                                 Nothing ->
                                     -- this is not possible case
                                     Cmd.none

                     Ok ( candidates, Just ( idString, relPos ) ) ->
                         let
                             mbCandi = List.head candidates

                             setTargetOption : (Option vt msg) ->
                                               (Result Error (Float, Float)) ->
                                               (Msg vt msg)
 
                             setTargetOption candi resOfGetStartAndRelPos
                                 = case resOfGetStartAndRelPos of
                                       Ok (startPos, candiRelPos) ->
                                           if abs candiRelPos <= abs relPos then
                                                   -- compare to previous record
                                                   -- and check next one to decide
                                                   DetermineTargetOption <|
                                                       Ok ( candidates
                                                                |> List.drop 1
                                                          -- ^ Ok is needed because
                                                          -- not using Task.attempt
                                                          -- but using Task.perform
                                                          , Just
                                                                ( candi.idString
                                                                , candiRelPos
                                                                )
                                                          )
                                               else
                                                   -- use previous one as a target
                                                   SetSnapToTargetOption
                                                       idString startPos relPos
                                       _ ->
                                           NoOp -- probably needs log
                         in
                             case mbCandi of
                                 Just candi ->
                                     taskTargetOptionRelPos
                                         state.idString candi.idString
                                         |> Task.map2
                                            Tuple.pair (state.idString
                                                            |> taskGetViewportPos)
                                               
                                         |> Task.attempt
                                            (messageMap << setTargetOption candi)
 
                                 Nothing ->
                                     -- no more option
                                     -- use last one as target option
                                     (state.idString |> taskGetViewportPos)
                                            |> Task.attempt
                                               (\res ->
                                                    case res of
                                                        Ok startPos ->
                                                            messageMap <|
                                                                SetSnapToTargetOption
                                                                    idString startPos relPos
                                                        _ ->
                                                            messageMap <|
                                                                NoOp
                                               )

                     Err _ ->
                         -- may be log something ...
                         Cmd.none
               )

           SetSnapToTargetOption idString startPos relPos ->
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

           MoveToTargetOption optionIdString ->
               ( { state |
                   targetIdString
                       = Just optionIdString
                 }
               , ( taskGetViewportPos state.idString
                 , taskTargetOptionRelPos state.idString optionIdString )
                   |> (Util.uncurry <|
                           Task.map2 Tuple.pair
                      )
                   |> Task.attempt
                      (\res ->
                           case res of
                               Ok ( basePos, relPos ) ->
                                   messageMap <|
                                   SetSnapToTargetOption
                                   optionIdString basePos relPos
                               _ ->
                                   messageMap NoOp
                      )

               )
                   
           ScrollPickerSuccess option ->
               ( if state.targetIdString == (Just option.idString) then
                     state |> stopSnapping
                 else
                     state

               , Debug.log "success" Cmd.none                -- XXX : need to focus ??
               ) 

           ScrollPickerFailure _ ->
               ( state
               , Cmd.none                -- XXX : maybe log ??
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
                                 |> Maybe.map String.toFloat
                         else
                             Nothing

               in
                   case mbNewViewportPos of
                       Just (Just newViewportPos) ->
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
                                     -- this value is not quite synchronized with
                                     -- 'OnScroll' Msg which catches the event later
                                     -- than one or two Animation happed already.
                                     -- So we will follow the trace of Animation
                                     -- position to check any 'scroll' events made
                                     -- from the this module in the end.
                                        
                                 }
                               , Cmd.batch
                                     [ animCmd
                                     , ( if (state.finalTargetScrollPosMP
                                                |> isInScrollTraceHelper
                                                |> List.filter ((==) 0)
                                                |> List.length            ) > 1 then
                                             --^ found the same position more than
                                             --  once which probably mean
                                             --  the viewport already at the destination
                                             case state.targetIdString
                                                    |> Maybe.map
                                                       ((Util.flip Dict.get)
                                                            state.optionIdToRecordDict)
                                             of
                                                 Just (Just targetIdString) ->
                                                     Task.succeed <|
                                                         ScrollPickerSuccess
                                                         targetIdString
                                                         
                                                 _ ->
                                                     Task.fail <|
                                                         InvalidOptionId
                                                         state.targetIdString
                                         else
                                             Task.succeed <|
                                                 AnimateSnapping
                                                 newViewportPosMP
                                       )
                                       |> Task.attempt
                                          (\res ->
                                               case res of
                                                   Ok internalMsg ->
                                                       messageMap internalMsg
                                                   _ ->
                                                       messageMap NoOp)
                                     ]
                               )
                       _ ->
                           ( { state |
                               pseudoAnimState
                                   = newAnimState
                             }
                           , animCmd
                           )

           -- the Msg wheere acutally move the viewport
           AnimateSnapping scrollPosMP -> -- MP : in Milli Pixel
               ( state
               , taskSetViewport state.idString
                   (scrollPosMP |> fromMilliPixel)
                        |> Task.attempt
                           (always <| messageMap NoOp)
               )

           NoOp ->
               ( state, Cmd.none )

                   
-- -- -- SUBSCRIPTIONS -- -- --

{-| -}
subscriptionsWithHelper : (String -> Animation.Msg -> msg) ->
                          List { a |
                                 idString         : String
                               , pseudoAnimState  : Animation.Messenger.State msg
                               } ->
                          Sub msg

subscriptionsWithHelper animMessageMapWith pickerRecords
    = pickerRecords
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

**Important:** no animation will work withought subscriptions!!!
-}
subscriptionsWith : List { state |
                           idString                 : String
                         , lastScrollClock          : Time.Posix
                         , scrollTraceMP            : Set Int
                         , finalTargetScrollPosMP   : Int
                         , scrollStopCheckTime      : Int
                         , optionIdToRecordDict     : Dict String (Option vt msg)
                         , optionIds                : List String
                         , pseudoAnimState          : Animation.Messenger.State msg
                         , targetIdString           : Maybe String
                         } ->
                    { model |
                      messageMapWith : (String -> (Msg vt msg) -> msg)
                    } ->
                    Sub msg

subscriptionsWith pickerStates model
    = pickerStates
    |> subscriptionsWithHelper
       (\idString ->
            model.messageMapWith idString << Animate)


-- -- -- SIMPLE EXAMPLE -- -- --

{-| This module has internal state so all the message
required to wrap (or map) to the your own Msg type.

**Note:** Int type in (Msg *Int* ExampleMsg) is the type for options.
Please Checkout [`Option`](#option) for further information.
-}
type ExampleMsg
    = ScrollPickerMessage String (Msg Int ExampleMsg)


{-| There are two separate picker in this example, first picker will choose
hour value, second one will choose minute value. and to deal with proper model
with update, messageMapWith will take a id(String) as first argument.

**Note:** pickerDirection means scrolling [`Direction`](#direction). And the name of
messageMapWith, pickerDirection is used in the [`scrollPicker`](#scrollPicker)
so it might be handy if you keep the same name.
-}
type alias ExampleModel
    = { firstPickerState  : MinimalState Int ExampleMsg
      , secondPickerState : MinimalState Int ExampleMsg
      , messageMapWith    : String -> (Msg Int ExampleMsg) -> ExampleMsg
      , pickerDirection   : Direction
      , hourValue         : Int
      , minuteValue       : Int
      }


{- not used here
onScroll : msg -> Attribute msg
onScroll msg
    = HtmlEvents.on "scroll" (Decode.succeed msg)
    |> htmlAttribute
-}                      

{-| Initialise our example model. Each picker model can be initialised with
[`initMinimalState`](#initMinimalState) and [`setOptions`](#setOptions)

And you might need to set default value for the picker.
how the example does
-}
exampleInit : () -> ( ExampleModel, Cmd ExampleMsg )
exampleInit flags
    = ( { firstPickerState -- for hour value
              = initMinimalState "firstScrollPicker"
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
              = initMinimalState "secondScrollPicker"
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
