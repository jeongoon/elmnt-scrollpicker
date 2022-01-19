module Elmnt.ScrollPicker
        exposing ( Direction
                 , verticalDirection
                 , horizontalDirection
                 , Option
                 , WithoutExtraFields
                 , WithExtraOptionFields
                 , skelOption
                 , wrapOption
                 , getOptions
                 , getOptionIdString
                 , setOptions
                 , setScrollStopCheckTime
                 , defaultTheme
                 , State
                 , initState
                 , viewAsElement
                 , Msg (..)
                 , anyNewOptionSelected
                 , updateWith
                 , taskGotoOptionWithIdHelper
                 , taskGotoOptionWithIndexHelper
                 , taskUpdateOptionLengthsHelper
                 , subscriptionsWith
                 )


{-| This module is an extened scroll picker from BaseScrollPicker

This module supports that each option item could have different attribute(s)
depends on the location of item which can be useful more effect for
scroll picker.

[example]: https://github.com/jeongoon/elmnt-scrollpicker/tree/3.0.0/examples/ClockTeller2.elm

-}

import Dict                                     exposing (Dict)
import Set                                      exposing (Set)
import Time
--import Process
import Task                                     exposing ( Task )

import Element                                  exposing (..)
-- ^ this module is basically based on the elm-ui

import Element.Font             as Font
import Element.Input            as Input
import Element.Border           as Border
import Element.Background       as Background

--import Color                                    exposing ( Color )

--import Html                                     exposing ( Html )

--import Json.Decode              as Decode

--import Css
--import Html.Styled
--import Html.Styled.Attributes                   exposing ( css )
--import Html.Styled.Events

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


-- -- -- Data types -- -- --

{-| you can hand it over to OptionLike or StateLike

```elm
type alias ExampleOption
  = Elmnt.BaseScrollPicker WithoutExtraFields Int ExampleMsg
```
-}
type alias WithoutExtraFields = {}

{-| Option type can be described as below as well
```elm
type alias Option vt msg
    = Base.MinimalOptionLike WithoutExtraFields vt msg
```
-}
type alias WithExtraOptionFields
    = { surfaceLength : Maybe Float
      }

{-| Option type for ScrollPicker
 -}
type alias Option vt msg
    = Base.MinimalOptionLike WithExtraOptionFields vt msg

type alias Direction
    = Base.Direction

{-| return Vertical of [`Direction`](#Direction)
-}
verticalDirection : Base.Direction
verticalDirection
    = Base.Vertical

{-| return Vertical of [`Direction`](#Direction)
-}
horizontalDirection : Base.Direction
horizontalDirection
    = Base.Horizontal

{-| -}
type OptionLengthInfoStatus
    = OptionLengthUnknown
    | OptionLengthReading
    | OptionLengthUpdated


{-| -}
type VectorDirection
    = North | East | South | West


{-| -}
type AngleDirection
    = ClockWise
    | CounterClockWise

-- -- -- State (Model) -- -- --

{-| used for internal type checking -}
type alias State vt msg
    = Base.MinimalStateLike
      { cssPerspectiveValue     : Float
      , optionLengthInfoStatus  : OptionLengthInfoStatus
      }
      WithExtraOptionFields
      vt msg

-- -- -- Helper functions -- -- --

{-| -}
uvecFromAngleDirection : AngleDirection -> number
uvecFromAngleDirection angDir
    = case angDir of
          ClockWise ->
              1
          CounterClockWise ->
              -1

{-| -}
type CurveAngle
    = Radian Float -- note: clock-wise: positive, ccw: negative
    | RightAngleGE -- GE: Greater than or Equal
                   --     consider as flat on North | West
    | RightAngleLE -- LE: Less than or Equal
                   --     consider as flat on South | East

{-| See more information about [`BaseMsg`][BaseMsg]

[BaseMsg]: /packages/jeongoon/elmnt-scrollpicker/latest/#Msg
-}
type Msg totalExtraOpt vt msg
    = BaseMsg (Base.Msg totalExtraOpt vt msg)
    | UpdateOptionLengths (Result (Error vt msg) (Option vt msg))
                          (List (Option vt msg))

{-| Error and some information type.

some type could be also handled with (ScrollPickerFailure : Base.Msg) message.
-}
type Error vt msg
    = BaseError Base.Error
    | InfoNoOption
    | GetOptionLengthFailure (Option vt msg)


{-| -}
allOptionLengthInfoReady : { stateWith |
                             optionLengthInfoStatus : OptionLengthInfoStatus
                           } ->
                           Bool
allOptionLengthInfoReady state
    = state.optionLengthInfoStatus == OptionLengthUpdated


{-| modify messageMapWith suitable for the BaseScrollPicker module.
-}
appModelForBase : { appModelWith |
                    messageMapWith : (String ->
                                          (Msg WithExtraOptionFields vt msg) ->
                                          msg)
                  , pickerDirection : Base.Direction
                  } ->
                  { messageMapWith : (String ->
                                          (Base.Msg
                                               WithExtraOptionFields vt msg) ->
                                          msg)
                  , pickerDirection : Base.Direction
                  }

appModelForBase appModel
    = { messageMapWith
            = (\idString baseMsg ->
                   appModel.messageMapWith idString <| BaseMsg baseMsg)
      , pickerDirection
            = appModel.pickerDirection
      }


taskGotoOptionWithIdHelper
    : { appModelWith |
        messageMapWith : (String ->
                              (Msg WithExtraOptionFields vt msg) -> msg)
      , pickerDirection : Base.Direction
      } ->
      String ->
      Maybe (Base.Error -> msg) ->
      State vt msg ->
      Task Never msg

taskGotoOptionWithIdHelper appModel
    = Base.taskGotoOptionWithIdHelper
      (appModel |> appModelForBase)


taskGotoOptionWithIndexHelper appModel optionIndex mbErrorHandler state
    = Base.taskGotoOptionWithIndexHelper
      (appModel |> appModelForBase) optionIndex mbErrorHandler state


{-| -}
taskUpdateOptionLengthsHelper
    : { appModelWith |
        messageMapWith : (String ->
                              (Msg WithExtraOptionFields vt msg) -> msg)
      , pickerDirection : Base.Direction
      } ->
      State vt msg ->
      Task Never msg

taskUpdateOptionLengthsHelper appModel state
    = let options
              = state |> getOptions
      in 
          UpdateOptionLengths
              (Err InfoNoOption)
              options
            |> appModel.messageMapWith state.idString
            |> Task.succeed

-- -- -- Init -- -- --

initState : String ->
            State vt msg

initState idString
    = initStateHelper idString Theme.Default

initStateHelper : String ->
                  (Theme.Value Float) ->
                  State vt msg
initStateHelper idString cssPerspectiveThemeValue
    = let minSt
              = Base.initMinimalState idString
      in
          { idString                = minSt.idString
          , optionIds               = minSt.optionIds
          , optionIdToRecordDict    = minSt.optionIdToRecordDict
          , targetIdString          = minSt.targetIdString
          , pseudoAnimState         = minSt.pseudoAnimState
          , lastScrollClock         = minSt.lastScrollClock
          , scrollTraceMP           = minSt.scrollTraceMP
          , finalTargetScrollPosMP  = minSt.finalTargetScrollPosMP
          , scrollStopCheckTime     = minSt.scrollStopCheckTime
          , optionIdInTheCenter     = minSt.optionIdInTheCenter
          , frameCenterPos          = minSt.frameCenterPos
          , optionCenterRelPos      = minSt.optionCenterRelPos

          , cssPerspectiveValue
                = cssPerspectiveThemeValue
                |> Theme.withDefault 500

          , optionLengthInfoStatus
              = OptionLengthUnknown
          }

{-| -}
wrapOption : Option vt msg ->
             OptionItem WithExtraOptionFields vt msg
wrapOption
    = Base.wrapOption

{-| -}
getOptions : State vt msg ->
             List (Option vt msg)
getOptions state
    = state
      |> Base.getOptions

{-| -}
getOptionIdString
    = Base.getOptionIdString

-- -- -- Update -- -- --

skelOption : { o |
               value         : vt
             , element       : (Element msg)
             , surfaceLength : (Maybe Float)
             } ->
             Option vt msg

skelOption { value, element, surfaceLength }
    = { idString
            = ""
      , index
            = -1
      , value
          = value
      , element
          = element
      , surfaceLength
          = surfaceLength
      }

{-| -}
setOptions : List ( String, OptionItem WithExtraOptionFields vt msg ) ->
             State vt msg ->
             State vt msg

setOptions subIdToOptionPairs state
    = { state |
        optionLengthInfoStatus
            = OptionLengthUnknown
      }
    |> Base.setOptions subIdToOptionPairs

{-| -}
setScrollStopCheckTime
    = Base.setScrollStopCheckTime

{-| Check the Msg, and return if there is any new selected option

please check this [`Example`][example] to see how you could use of it.
-}
anyNewOptionSelected : Msg WithExtraOptionFields vt msg ->
                       Maybe (Option vt msg)
anyNewOptionSelected msg
    = case msg of
          BaseMsg (ScrollPickerSuccess option) ->
              Just option
          _ ->
              Nothing

{-| update function needs extra info which are `messageWith` and
`pickerDirection`.

Main difference form Base.updateWith is that every scroll will update
which option is located near to the center and how far from it and that
picker could get the pyhsical length (height or width depends on
the `pickerDirection`) of each option via `UpdateOptionLengths` message.
-}
updateWith : { appModelWith |
               messageMapWith : (String -> (Msg WithExtraOptionFields vt msg) -> msg)
             , pickerDirection : Direction
             } ->
             Msg WithExtraOptionFields vt msg ->
             State vt msg ->
             ( State vt msg
             , Cmd msg
             )

updateWith ( { messageMapWith, pickerDirection } as appModel ) msg state
    = let
        messageMap
            = messageMapWith state.idString

        ( posAccessor, lengthAccessor )
            = pickerDirection
            |> Base.getPosAndLengthAccessors

  in
       case msg of
           BaseMsg OnScroll ->
               -- FIXME : optionlengh ready?
               state
                   |> Base.updateWith
                      (appModel |> appModelForBase) Base.OnScroll

                   |> Tuple.mapSecond
                      (\origCmd ->
                           Cmd.batch
                           [ origCmd
                           , Task.perform
                             (messageMap << BaseMsg << Base.FindCenterOption)
                              -- v on success
                             (Task.succeed <| Base.SyncCenterOption)
                           -- find option in the center for every scroll
                           -- in order to update attributes for option
                           -- at every position
                           ]
                      )

           BaseMsg baseMsg ->
               -- FIXME : optionlengh ready?
               state
                   |> Base.updateWith
                      (appModel |> appModelForBase) baseMsg

           UpdateOptionLengths resChangedOption moreOptions ->
               ( { state |
                   optionLengthInfoStatus
                       = if List.isEmpty moreOptions then
                             OptionLengthUpdated
                         else
                             OptionLengthReading
                 }
                 |> ( case resChangedOption of
                          Ok changedOption ->
                              replaceOption
                              ( Base.wrapOption changedOption )

                          Err _ ->
                              identity
                    )

               , ( case List.head moreOptions of
                       Just option ->
                           Browser.Dom.getElement option.idString
                                |> Task.mapError
                                   (\_ -> GetOptionLengthFailure option)
                                |> Task.attempt
                                   (\res ->
                                        messageMap <|
                                        UpdateOptionLengths
                                        ( case res of
                                              Ok domElement ->
                                                  Ok <|
                                                  { option |
                                                    surfaceLength
                                                        = domElement
                                                        |> .element
                                                        |> lengthAccessor
                                                        |> Just
                                                  }
                                              Err _ ->
                                                  Ok <|
                                                  { option |
                                                    surfaceLength
                                                        = Nothing
                                                  }
                                        )
                                        ( moreOptions
                                            |> List.drop 1
                                        )
                                   )
                       Nothing ->
                           Task.perform
                               (messageMap << BaseMsg) (Task.succeed Base.NoOp)
                 )
                 |> (\cmdForUpdating ->
                         case resChangedOption of
                             Ok _ ->
                                 cmdForUpdating

                             Err error ->
                                 Cmd.batch
                                 [ cmdForUpdating
                                 , Task.perform messageMap
                                     ( Task.succeed <|
                                           BaseMsg <|
                                           Base.ScrollPickerFailure
                                           ( state.idString ++ "|>"
                                             ++ "ScrollPicker.UpdateOptionLengths" )
                                           "Dom.getElement had failed."
                                           Base.ErrorMessageOnly
                                     )
                                 ]
                    )
               )


{-| get information related to an option -- especially relative location from
the beginning of option list -- and give a list of Attribute for the option and
updated internal state to help get the attribute for next option.
-}
type alias OptionViewFunction vt msg palette internalStateType
    = { pickerDirection   : Base.Direction
      , pickerLengthFloat : Float
      , option            : Option vt msg
      , framePosition     : Base.TwoDim
      , optionRelPosition : Base.TwoDim
        -- note:^ relative position from center of frame
      , optionAngleStart  : CurveAngle
      , optionAngleEnd    : CurveAngle
      , palette           : palette
      } ->
      (List (Element msg), internalStateType) ->
      -- ^ could be angle or distance ...
      (List (Element msg), internalStateType )


{-| Theme for ScrollPicker which has function for generate elm-ui attributes
for each option item.
-}
type alias Theme vt msg palette internalStateType
    = Base.BaseThemeLike
      { viewOptionElementFn
            : Theme.Value
              (OptionViewFunction vt msg palette internalStateType)
      }
      palette msg

{-| default theme for this ScrollPicker
-}
defaultTheme : Theme vt msg Palette internalStateType
defaultTheme
    = let
        df = Theme.Default
   in
       { palette
             = Theme.defaultPalette
       , borderWidth         = df
       , borderColorFn       = df
       , shadingColorFn      = df
       , focusColorFn        = df
       , backgroundColorFn   = df
       , fontColorFn         = df
       , fontSize            = df
       , shadeLength         = df
       , pickerLength        = df
       , pickerWidth         = df
       , shadeAttrsFn        = df
       , viewOptionElementFn = df
       }


{-| FIXME
-}
viewAsElement
    : { appModel |
        messageMapWith : (String -> (Msg WithExtraOptionFields vt msg) -> msg)
      , pickerDirection : Base.Direction
      } ->
      ( Theme
            vt msg
            (Base.MinimalPaletteLike pal
                 (Base.MinimalPaletteOnLike palOn))
            { mbTotalProjectedRelPos : Maybe Float }
      ) ->
      State vt msg ->
      Element msg

viewAsElement appModel theme state
    = let
        pickerDirection
            = appModel.pickerDirection

        { lengthSetter, widthSetter, centerLateral, shadeLength,
          fontSize, borderWidth, pickerWidth, pickerLength } =

            Base.defaultBaseSettingsWith theme pickerDirection

        makeTwoDimWith : Float -> Float -> Base.TwoDim
        makeTwoDimWith centerPos projectedLength
            = let halfLength
                      = projectedLength
                      |> abs -- so that we can make
                             -- startPoint is lower value correctly
                      |> \n -> n / 2
              in
                    { startPoint
                          = centerPos - halfLength
                    , endPoint
                          = centerPos + halfLength
                    }

        -- divide options as prior options, center option and following options.
        mbPartitionOptions opts
            = state.optionIdInTheCenter
            |> Maybe.andThen
               (\optId ->
                    state.optionIdToRecordDict
                    |> Dict.get optId
                    |> Maybe.map Base.unwrapOption
               )
            |> Maybe.andThen
               (\centerOpt ->
                    state
                        |> Base.getOptions
                        |> List.partition (\o -> o.index < centerOpt.index)
                        |> (\(former, latter) ->
                                ( former
                                , centerOpt
                                , latter
                                    |> List.drop 1 -- remove center option
                                )
                           )
                        |> Just
               )

        pickerCurveRadiusWith perspec
            = {- in relatation between two triangle ...

                         radius                 picker length
                 ---------------------  = -----------------------
                  perspective + radius         2 * perspective
              -}
              let pLength
                      = pickerLength |> toFloat
              in
                  pLength * perspec
                      / ( 2 * perspec - pLength)

        -- make option element list (Element.Element)
        -- which has two parts
        -- 1. determine the option is on the pseudo on the curve
        --    of picker view frame or just on the flat surface
        -- 2. make each Element based on the data of option
        --    in point of view of `perspective-origin`

        mbOptionElements
            = if state |> allOptionLengthInfoReady then
                  justOptionElements
              else
                  Nothing
                  -- ^ BaseScrollPicker will generate the elements

        justOptionElements
            = state
              |> Base.getOptions
              |> mbPartitionOptions
              |> Maybe.andThen
                 (\( priorOptsOnly, centerOpt, nextOptsOnly ) ->
                      let
                          {-
                              P : css perspective value ( > 0)
                              R : radius
                              υ : optionCenterRelPos
                                  (when watch from the perspective distance)

                              ν : real option relative position
                                  from the center of curve

                              P : υ = P + R - R cos α : ν

                                     υ ( P + R (1 - cos α))
                              ν = ----------------------------
                                            P

                              where
                                            - υ
                                  sin α = -------
                                              R
                                  α = asin ( -υ / R )
                          -}
                          perspec
                              = state.cssPerspectiveValue

                          radius
                              = pickerCurveRadiusWith perspec

                          upsilon
                              = let _ = Debug.log "id" state.optionIdInTheCenter
                                in
                                    Debug.log "upsilon" state.optionCenterRelPos  -- still has sign

                          alpha
                              = -upsilon / radius
                              |> asin
                              |> Debug.log "alpha"

                          nu
                              = upsilon * ( perspec + radius * (1 - cos alpha))
                                / perspec

                          l = centerOpt.surfaceLength
                            |> Maybe.withDefault ((pickerLength |> toFloat) / 3)


                          angleSign
                              = if alpha < 0 then
                                    -1
                                else
                                    1
                          {-
                              θ : initial angle for the closer point of
                                  option to the x-axis  (has same sign as α)

                              θ = α + (-angleSign) β

                                       l
                              tan β = ----
                                       2R

                              κ : initial closer point position of option
                                   to the x-axis

                              κ = ν + cos α (α * ν < 0 : has oppsite sign)

                         -}
                          beta
                              = l / (2 * radius)
                              |> atan

                          theta
                              = alpha + (-angleSign) * beta

                          kappa
                              = nu + cos alpha

                          initInternalState
                          -- ^ make initial states for both option list
                              = ( [] -- empty list
                                , { optionAngleStart
                                        = Radian theta

                                  , optionRelPosStart
                                        = kappa
                                  }
                                )

                          ( priorOpts, nextOpts )
                              = if angleSign < 0 then
                                    -- center option close to the
                                    -- North (Horizontal) or West (Vertical)
                                    ( priorOptsOnly ++ [ centerOpt ]
                                    , nextOptsOnly
                                    )

                                else
                                    -- center option close to the
                                    -- South (Horizontal) or East (Vertical)
                                    ( priorOptsOnly
                                    , centerOpt :: nextOptsOnly
                                    )

                          discoverOptionOnSurface
                              = discoverOptionOnSurfaceHelper radius

                          framePosition
                              = makeTwoDimWith
                                state.frameCenterPos
                                (pickerLength |> toFloat)

                          applyViewOptionElement (discoveredOpts, _)
                              = discoveredOpts
                             |> List.map
                               (\{ option, optionRelPosition
                                 , optionAngleStart, optionAngleEnd } ->

                                    { pickerDirection
                                          = pickerDirection
                                    , pickerLengthFloat
                                          = pickerLength |> toFloat
                                    , option
                                          = option
                                    , framePosition
                                          = framePosition
                                    , optionRelPosition
                                          = optionRelPosition
                                    , optionAngleStart
                                          = optionAngleStart
                                    , optionAngleEnd
                                          = optionAngleEnd
                                    , palette
                                          = theme.palette
                                    }
                               )
                             |> List.foldr
                                (viewOptionElementFnHelper perspec)

                                -- v  initial accumulator for foldr
                                ( [] -- empty list
                                , { mbTotalProjectedRelPos
                                        {-
                                            p = state.cssPerspectiveValue
                                            r = radius

                                                        p : |x|
                                                          =
                                            p + r (1 - cos θ) - l |sin α|
                                             : nearer point from the center
                                               (κ)
                                           -}
                                        = perspec * kappa
                                        / ( perspec
                                              + radius *
                                                  (1 - cos theta)
                                              - l * (sin alpha
                                                           |> abs)
                                          )
                                        * angleSign
                                         |> Just
                                                 -- FIXME : change mbTotalProjectedRelPos to totalProjectedPos after checking data correct.
                                  }
                                )
                             |> Tuple.first


                     in
                         ( priorOpts
                              |> List.foldr
                                 (discoverOptionOnSurface ClockWise)
                                 initInternalState
                              |> applyViewOptionElement
                         )
                         ++
                         ( nextOpts
                                 |> List.foldl
                                    (discoverOptionOnSurface CounterClockWise)
                                    initInternalState
                                    -- applyViewOptionElement will use foldr
                                    -- don't need to reverse before but later.
                                 |> applyViewOptionElement
                                 |> List.reverse
                         )

                         |> Just
                 )


         {- find whether the option is on the curve or not
            by traversing option by option
           and will give the `real` position (not projected)
           as startPoint and endPoint and also give
           some hint of angle when the line from the center of option
           to the center of curve as clock wise value
        -}
        discoverOptionOnSurfaceHelper
            : Float ->
              AngleDirection ->
              Option vt msg ->

             ( List { option            : Option vt msg
                    , optionAngleStart  : CurveAngle
                    , optionAngleEnd    : CurveAngle
                    , optionRelPosition : Base.TwoDim
                    }
             , { optionAngleStart  : CurveAngle
               , optionRelPosStart : Float         -- relative position
                                                    -- from x-axis
               }
             ) ->
             ( List { option : Option vt msg
                    , optionAngleStart  : CurveAngle
                    , optionAngleEnd    : CurveAngle
                    , optionRelPosition : Base.TwoDim
                    }
             , { optionAngleStart  : CurveAngle
               , optionRelPosStart : Float
               }
             )

        discoverOptionOnSurfaceHelper radius angleDir
                                      currOpt
                                      ( discovered
                                      , { optionAngleStart
                                        , optionRelPosStart
                                        }
                                      )

            = let
                l = currOpt.surfaceLength
                  |> Maybe.withDefault ((pickerLength |> toFloat) /3) -- XXX

                angleSign
                    = uvecFromAngleDirection angleDir

                newDiscoveredOnFlat
                -- ^ for outside of curved area
                    = let currOptAngleStart
                              = case angleDir of
                                    ClockWise ->
                                        RightAngleGE
                                    CounterClockWise ->
                                        RightAngleLE

                          currOptRelPosStart
                              = -angleSign * l + optionRelPosStart
                      in
                          ( { option
                                  = currOpt

                            , optionRelPosition
                                  = Base.makeTwoDim
                                    optionRelPosStart
                                    currOptRelPosStart


                            , optionAngleStart
                                  = currOptAngleStart
                            , optionAngleEnd
                                  = currOptAngleStart -- not useful data here

                            } :: discovered

                          , { optionAngleStart
                                  = currOptAngleStart
                            , optionRelPosStart
                                  = currOptRelPosStart
                            }
                          )


           in
               case optionAngleStart of
                   Radian theta ->
                       let
                           {-
                               θ' = θ + 2β'

                                           l'
                               tan β' =  ------
                                           2R

                               ν' = - sin θ'

                            -}
                           beta1
                               = l / (2 * radius)
                               |> atan

                           theta1
                               =  theta + (angleSign * 2 * beta1)

                           nu1
                               = -(sin theta1)

                       in
                           if (theta1 |> abs) <= (pi/2) then
                               -- ^ option is on the curve

                               ( { option
                                       = currOpt

                                 , optionRelPosition
                                       = Base.makeTwoDim
                                         optionRelPosStart
                                         nu1

                                 , optionAngleStart
                                       = Radian theta
                                 , optionAngleEnd
                                       = Radian theta1

                                 } :: discovered

                               -- v  states for next interation
                               , { optionAngleStart
                                       = Radian theta1
                                 , optionRelPosStart
                                       = nu1
                                 }
                               )
                           else
                               -- not on the curve: on float plane
                               newDiscoveredOnFlat
                   _ ->
                       newDiscoveredOnFlat

        -- this function will serve higher order function like foldl, foldr
        viewOptionElementFnHelper
            : Float ->
              (OptionViewFunction vt msg
                   (Base.MinimalPaletteLike pal
                        (Base.MinimalPaletteOnLike palOn))
                   { mbTotalProjectedRelPos : Maybe Float } -- internalStateType
              )

        viewOptionElementFnHelper (cssPerspectiveValue as perspec)
            = theme.viewOptionElementFn
            |> Theme.withDefault
               (\{ {-pickerDirection,-} pickerLengthFloat, framePosition,
                   option, optionRelPosition, optionAngleStart, optionAngleEnd,
                   palette
                 }
                 ( elements, { mbTotalProjectedRelPos } ) ->

                    let
                        theta
                            = case optionAngleStart of
                                  Radian rad ->
                                      rad
                                  _ ->
                                      0

                        theta1
                            = case optionAngleEnd of
                                  Radian rad ->
                                      rad
                                  _ ->
                                      0

                        radius
                            = pickerCurveRadiusWith perspec

                        centerPosOf twoDim
                            = twoDim.endPoint + twoDim.startPoint
                            |> \n -> n / 2

                        distanceOf twoDim
                            = twoDim.endPoint - twoDim.startPoint

                        {- noteOptionElementEquation

                           now every option position need to be recalculated
                           in perspective view.

                            "origin" : the center of the curve

                            Ρ(rho) = perspec + radius
                            τ = shorter distance from the "origin" to option
                            ν = longer distance from the "origin" to option

                            note: τ, σ, μ, ε  have sign along with the value

                          in small triangle ...

                            perspec : μ = κ : τ

                                  perspec * τ
                            μ = --------------
                                      κ

                            σ = Ρ - (R * tan θ')    ( σ > 0 )
                          --^ bottom line in big triangle

                            κ = Ρ - (R * tan θ)
                          --^ bottom line in small triangle


                         in big triangle
                            perspec : ε = σ : ν

                                  perspec * ν
                            ε = --------------
                                      σ

                           we need to find out  μ, ε   |μ| < |ε|
                              as relative position (from the perspective bottom
                              line) of projected option endPoint & startPoint
                        -}
                        rho
                            = perspec + radius

                        l   = option.surfaceLength
                            |> Maybe.withDefault (pickerLengthFloat / 3) -- XXX


                        ( longerPointAccessor, shorterPointAccessor )
                            = if theta >= 0 then
                                  ( .startPoint, .endPoint )
                              else
                                  ( .endPoint, .startPoint )
                        tau
                            = optionRelPosition
                            |> shorterPointAccessor

                        nu
                            = optionRelPosition
                            |> longerPointAccessor

                        sigma -- sigma > 0
                            = rho - ( radius * tan (theta |> abs) )

                        gamma
                            = pi / 2 - theta + (( theta + theta1 ) / 2)

                        rotateXValue
                            = -gamma
                            |> String.fromFloat
                            |> \s -> s ++ "rad"

                        posDirectionOf relpos
                            = case pickerDirection of
                                  Base.Horizontal ->
                                      if relpos < 0 then
                                          North
                                      else
                                          South
                                  Base.Vertical ->
                                      if relpos < 0 then
                                          West
                                      else
                                          South

                        toPxString floatVal
                          = floatVal
                          |> String.fromFloat
                          |> \s -> s ++ "px"

                        perspecLo posDir -- Lo : Longitudinal Origin
                            = ( case posDir of
                                    North ->
                                        (abs tau)
                                    West ->
                                        (abs tau)
                                    South ->
                                        (abs tau) * -1
                                    East ->
                                        (abs tau) * -1
                              )
                              |> toPxString

                        originPair optProjPos
                           = case ( posDirectionOf (centerPosOf optProjPos)
                                  , pickerDirection
                                  )
                             of
                                 ( North, Vertical ) ->
                                     ( "50% "++ (perspecLo North), "50% 100%" )

                                 ( _, Vertical ) ->
                                     ( "50% " ++ (perspecLo South), "50% 0%")

                                 ( West, Horizontal ) ->
                                     ( (perspecLo West) ++" 50%", "100% 50%" )

                                 ( _, Horizontal ) ->
                                     ( (perspecLo East) ++ " 50%", "0% 50%" )

                    in
                        if theta == 0 then
                            -- draw the option as hidden and flat
                            ( ( el [ option.idString |> MAttr.id
                                   , l |> round |> px |> lengthSetter
                                   , fill |> widthSetter
                                   , centerLateral
                                   ] <|
                                el [ centerX
                                   , centerY
                                   ] option.element
                              ) :: elements

                            , { mbTotalProjectedRelPos
                                    = Nothing
                              }
                            )

                        else
                            let
                                mu -- see above μ : sign same as tau
                                    = case mbTotalProjectedRelPos of
                                          Nothing ->
                                          -- FIXME
                                          -- actually this is not happening
                                          -- remove after checking
                                              let kappa -- kappa > 0
                                                      = rho
                                                        - ( radius
                                                                * tan (theta
                                                                        |>abs) )

                                              in
                                                  perspec * tau / kappa

                                          Just prevEpsilon ->
                                              prevEpsilon
                                              -- ^ literally previous `epsilon`
                                              --   becomes current `mu`

                                epsilon -- sign same as nu
                                    = perspec * nu / sigma

                                newProjectedRelTwoDim
                                    = Base.makeTwoDim mu epsilon

                                optionProjLength
                                    = distanceOf newProjectedRelTwoDim

                                ( perspecOrigin, transformOrigin )
                                    = originPair newProjectedRelTwoDim

                                -- parent div need to be shrink
                                -- so child div might need to adjust margin
                                -- on top, especially when transform origin
                                -- is at the bottom(for Horizontal) or right
                                mbMarginTopValue posDir
                                    = if posDir == North ||
                                         posDir == West then
                                          optionProjLength
                                             |> \n -> n - l
                                             |> toPxString
                                             |> Just
                                      else
                                          Nothing

                            in
                                ( ( el [ option.idString |> MAttr.id
                                       , optionProjLength
                                           |> round |> px |> lengthSetter
                                       , fill |> widthSetter
                                       , MAttr.passPointerEvents
                                       , MAttr.style "perspective"
                                           (state.cssPerspectiveValue
                                               |> toPxString)
                                       , MAttr.style
                                             "perspective-origin"
                                             perspecOrigin
                                       ] <|

                                    el ( List.filterMap identity <|
                                         [ MAttr.positionAbsolute |> Just

                                         , mbMarginTopValue
                                               (posDirectionOf <|
                                                    centerPosOf <|
                                                    newProjectedRelTwoDim)
                                             |> Maybe.map
                                                (MAttr.style "margin-top")

                                         , l -- option.surfaceLength
                                             |> round |> px |> lengthSetter
                                             |> Just

                                         , fill |> widthSetter |> Just

                                         , MAttr.style "transform-style"
                                             "preserve-3d" |> Just

                                         , MAttr.style "transform"
                                             ( "rotateX(" ++
                                                   rotateXValue ++ ")"
                                             ) |> Just

                                         , MAttr.style "transform-origin"
                                             transformOrigin
                                           |> Just
                                         ]
                                       ) <|
                                    el [ centerX
                                       , centerY
                                       ] option.element
                                  ) :: elements

                                , { mbTotalProjectedRelPos
                                        = Just epsilon
                                  }
                                )
               )

   in
       -- FIXME: make default theme here ...
       state
           |> Base.viewAsElementHelper
              ( appModel
                  |> appModelForBase )
              theme
              mbOptionElements

subscriptionsWith pickerStates appModel
    = Base.subscriptionsWith pickerStates (appModel |> appModelForBase)
