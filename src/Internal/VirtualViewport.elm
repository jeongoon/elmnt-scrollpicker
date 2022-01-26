module Internal.VirtualViewport
    exposing ( SanityOk
             , SanityUnknown
             , Pos
             , emptyPosWith
             , fromPos
             , getViewportPosValue
             , getLastClockTime
             , PosRecord
             , Settings
             , toSettings
             , toElementLength
             , PageLength
             , makePageLength
             , fromPageLength
             , PageNum
             , toInitPageNum
             , fromPageNum
             , ViewportPos
             , makeViewportPos
             , fromViewportPos
             , adjustViewportPos
             , WarpInfo
             , getMaybeWarpInfo
             , popMaybeWarpInfo
             , initPos
             , updatePosAndRealViewportPos
             , toRealViewportPosVal
             )

{-| This module is used in `BaseScrollPicker` only. `BaseScrollPicker` uses
a separate `Viewport` for taking scroll events.

To generate pseudo scrolling for *actual* `View` for user, `VirtualViewport`
is introduced because in this way we can implement the **looping** scrolling


Virtual Viewport looks like below

```
-------- +--------+---
^        |        | o    When enter A -> A'
|    --- +----B---+---   When enter B -> B'
|     p  |        | i
v     a  |        |
i     g  +----A'--+---   o : wormInMargin
e     e  |        |      i : wormOutPadding
w        |        |          (padding: because pageLength include `i` part)
      l  |        |
p     e  |        |
o     n  |        |
r     g  +----B'--+
t     t  |        |
|     h  |        |
|    --- +----A---+     virtualPageLength = distance between `A` and `B`
v        |        |
-------- +--------+

note: however normally viewport is not a point but has length(height)
exact calculation can be seen in `adjustViewportPos`

-}

import Time


{-| for phanthom type of most of functions here `SanityOk` means, the value is
 ready to use for now.
-}
type SanityOk
    = SanityOk

{-| for phanthom type of most of functions here `SanityUnknown` means, the value
 haden't been checked for its sanity yet. Normally a value newly created has
the type of `SanityUnknown`.

````elm
-- the module imported as `V`
V.makePageLength 1000
-- -> PageLength 1000 : V.PageLength V.SanityUnknown
```
-}
type SanityUnknown
    = SanityUnknown

{-| data for a `VirtualViewport` value
-}
type Pos compatibleSanity viewportCompatibleSanity
    = Pos (PosRecord viewportCompatibleSanity)


{-| exported for debugging purpose or quick use
 -}
fromPos : Pos compatibleSanity viewportCompatibleSanity
          -> PosRecord viewportCompatibleSanity
fromPos (Pos posRec)
    = posRec

{-| get current viewport position in Float value
-}
getViewportPosValue : Pos compatibleSanity viewportCompatibleSanity
                      -> Float
getViewportPosValue (Pos posRec)
    = posRec.lastViewportPos
    |> fromViewportPos

{-| retreive `lastClock` from the `Pos`
-}
getLastClockTime : Pos compatibleSanity viewportCompatibleSanity ->
                   Int
getLastClockTime (Pos posRec)
    = posRec.lastClockTime

    
{-| retreive `WarpInfo` from the `Pos`
-}
getMaybeWarpInfo : Pos compatibleSanity viewportCompatibleSanity ->
                   Maybe WarpInfo
getMaybeWarpInfo (Pos posRec)
    = posRec.mbLastWarpInfo



{-| Internal structure for `Pos`

exported for debugging purpose.
 -}
type alias PosRecord viewportCompatibleSanity
    = { virtualPageNum     : PageNum SanityOk
      , lastViewportPos    : ViewportPos viewportCompatibleSanity
      , lastClockTime      : Int
      , lastViewportOffset : Float
      , mbLastWarpInfo     : Maybe WarpInfo
      }


{-| `Settings` for VirtualViewport. you can create valid `Settings`
by calling `toSettings`
-}
type Settings sanityChecked
    = Settings InternalSettingsRecord

{-| -}
type alias InternalSettingsRecord
    = SettingsRecordBase SanityOk


{-| There is no builder for this type. a record syntax is required to
 create one.
-}
type alias SettingsRecordBase compatibleSanity
    = { virtualPageLength : PageLength compatibleSanity
      , wormOutPadding    : Float
      , wormInMargin      : Float
      }

{-| make a valiad settings with `SettingsRecordBase` data type. -}
toSettings : SettingsRecordBase SanityUnknown ->
             Settings SanityOk
toSettings { virtualPageLength,
             wormOutPadding,  wormInMargin }
    = let
        pageLengthValue
            = virtualPageLength |> fromPageLength

        acceptedWormOutPadding
            = wormOutPadding
            |> min 150

        acceptedWormInMargin
            = wormInMargin
            |> min 100
            |> min acceptedWormOutPadding

        acceptedPageLengthVal
            = pageLengthValue
            |> max ( 3 * wormOutPadding )

   in
       { virtualPageLength
             = acceptedPageLengthVal
             |> okPageLength
       , wormOutPadding
             = acceptedWormOutPadding
       , wormInMargin
             = acceptedWormInMargin
       }
       |> \b -> Settings b


{-| To create *actual* viewport element, this function will give
the size of it. (note: in Float value)
-}
toElementLength : Settings SanityOk ->
                  Float
toElementLength (Settings { virtualPageLength, wormInMargin })
    = (virtualPageLength |> fromPageLength ) + wormInMargin * 2


{-| a Float type of length of `VirtualViewport` page
normally `VirtualViewport` is smaller than the real viewport is,
paging is required to map the correct position.
-}
type PageLength sanityChecked
    = PageLength Float

{-| must not be exported
 -}
okPageLength : Float ->
               PageLength SanityOk
okPageLength
    = PageLength

{-| make an initial and `SanityUnknown` value of `PageLength`
-}
makePageLength : Float -> PageLength SanityUnknown
makePageLength
    = PageLength

{-| drop the container of PageLength and give float value.
 -}
fromPageLength : PageLength compatibleSanity -> Float
fromPageLength (PageLength value)
    = value

{-| Page number value with sanity checking phanthom type
 -}
type PageNum sanityChecked
    = PageNum Int

{-| must not be exported
 -}
okPageNum : Int -> PageNum SanityOk
okPageNum
    = PageNum

{-| make a page number which is exactly fit into current `Settings`

the page number will be bounded from `0` to exact maximum page number.
 -}
toInitPageNum : Settings SanityOk -> Float ->
                PageNum SanityOk

toInitPageNum ((Settings { virtualPageLength }) as settings) lastViewportPos
    = lastViewportPos
      / (virtualPageLength |> fromPageLength)
    |> ceiling
    |> \n -> n - 1
    |> PageNum


{-| not exported

note: `-1` will keep generating negative value
    (max page number + 1) will keep generating maximum value
-}
boundPageNum : Settings SanityOk -> Float -> Int ->
               PageNum SanityOk

boundPageNum (Settings { virtualPageLength })
             lastSceneLength pageNumVal
    = let
        oneMoreThanMaxPageNumVal
            = lastSceneLength / (virtualPageLength |> fromPageLength)
            |> ceiling
   in
       pageNumVal
           |> max -1
           |> min oneMoreThanMaxPageNumVal
           |> PageNum

{-| Drop the container and give page number value as `Int`
 -}
fromPageNum : PageNum compatibleSanity -> Int
fromPageNum (PageNum value)
    = value


{-| Virtual Viewport position value with `sanity` check phanthom type.
 -}
type ViewportPos sanityChecked
    = ViewportPos Float


{-| must not be exported
-}
okViewportPos : Float -> ViewportPos SanityOk
okViewportPos
    = ViewportPos


{-| make a ViewportPos type without sanity-check. -}
makeViewportPos : Float -> ViewportPos SanityUnknown
makeViewportPos
    = ViewportPos


{-| Drop the container and give viewport position value as `Float`
 -}
fromViewportPos : ViewportPos compatibleSanity -> Float
fromViewportPos (ViewportPos value)
    = value


{-| must not be exported
-}
boundedViewportPosVal : Settings SanityOk ->
                        Float -> Float
boundedViewportPosVal settings posVal
    = posVal
    |> max 0
    |> min ( settings |> toElementLength )


{-| check the viewport position is on *warp zone* and inform
the relative page number and recalculated viewport position
together.
-}
adjustViewportPos
    : Settings SanityOk ->
      { stateWith |
        lastViewportLength : Float
      } ->
      ViewportPos SanityUnknown ->
      ( Int, ViewportPos SanityOk )

adjustViewportPos ((Settings { virtualPageLength,
                               wormInMargin, wormOutPadding })
                       as settings)
                  ({ lastViewportLength } as state)
                  (ViewportPos vposValOrig)
    = let vposValLower
              = vposValOrig
              |> boundedViewportPosVal settings

          vposValHigher
              = vposValLower
                + lastViewportLength

          pageLength
              = virtualPageLength |> fromPageLength

          elementLength
              = settings
              |> toElementLength

          higherWormInMarginAbs
              = wormInMargin + pageLength -- please see picture on top

          ( adjustPageAmount, vposVal1 )
              = if vposValLower <= wormInMargin then
                    {- vposVal1 = pageLength + wormInMargin
                                  - wormOutPadding
                                  - (wormInMargin - vposValLower)
                                  - lastViewportLength
                       therefore ...
                    -}
                    ( -1 -- go previous page
                    , vposValLower + pageLength - wormOutPadding
                        - lastViewportLength
                    )
                else
                    if vposValHigher >= higherWormInMarginAbs then
                        {- vposVal1 = wormInMargin + wormOutPadding
                                   +  (vposValHigher - higherWormInMarginAbs)
                           therefore ...
                        -}
                        ( 1
                        , vposValHigher + wormOutPadding - pageLength
                        )
                    else
                        ( 0, vposValLower )
      in
          ( adjustPageAmount
          , vposVal1
              |> okViewportPos
          )

{-| if viewport position had to be changed, this type for the short record
for that.

note: if WarpInfo is a `Just` value in the `Pos`, `Pos`'s `lastViewportPos`
value is set to  (mbWarpInfo |> Maybe.map ( .to >> Tuple.mapSecond).
-}
type alias WarpInfo
    = { from : (PageNum SanityUnknown
               , ViewportPos SanityUnknown)

      , to   : (PageNum SanityOk
               , ViewportPos SanityOk)
      }


{-| retreive `WarpInfo` from the `Pos`
and teturn Pos withough `mbWarpInfo` field
-}
popMaybeWarpInfo : Pos compatibleSanity viewportCompatibleSanity ->
                   ( Maybe WarpInfo, Pos compatibleSanity viewportCompatibleSanity)
popMaybeWarpInfo (Pos posRec)
    = ( posRec.mbLastWarpInfo
      , Pos { posRec |
              mbLastWarpInfo
                  = Nothing
            }
      )

{-| must be not exported -}
sanitizePos : PosRecord compatibleSanity -> Pos SanityOk compatibleSanity
sanitizePos
    = Pos

{-|
-}
emptyPosWith : Settings SanityOk -> Pos SanityUnknown SanityOk
emptyPosWith (Settings { wormOutPadding })
    = Pos
      { virtualPageNum
            = PageNum 0
      , lastViewportPos
            = (ViewportPos wormOutPadding)
      , lastClockTime
            = 0
      , lastViewportOffset
            = 0
      , mbLastWarpInfo
            = Nothing
      }

{-| set valid and tidy `Pos` value with `Settings`, **Real Viewport State**
-- which the virtual viewport wants to map -- and new viewport position value
and time
-}
initPos : Settings SanityOk ->
          { stateWith |
            lastScrollClock : Time.Posix
          , lastViewportPos    : Float
          , lastViewportLength : Float
          , lastSceneLength    : Float
          } ->
          ( Time.Posix, ViewportPos SanityUnknown ) ->
          Pos SanityUnknown SanityOk

initPos ( (Settings { virtualPageLength }) as settings )
        ( { lastScrollClock, lastViewportPos }
              as state )
        newPosInfo
   = let
        initPageNum
            = lastViewportPos
            |> toInitPageNum settings

     in newPosInfo
         |> setPosHelper settings state
            { initPageNum
                  = initPageNum
            , mbViewportOffset
                  = Nothing
            }
         |> Pos


{-| must be not exported

check viewport
internal help function
-}
setPosHelper : Settings SanityOk ->
               { stateWith |
                 lastViewportPos    : Float
               , lastViewportLength : Float
               , lastSceneLength    : Float
               } ->
               { initPageNum : PageNum SanityOk
               , mbViewportOffset : Maybe Float
               } ->
               ( Time.Posix, ViewportPos SanityUnknown ) ->
               PosRecord SanityOk -- SanityOk is for lastViewportPos

setPosHelper ((Settings { virtualPageLength, wormInMargin })
                  as settings
             )
             ({ lastViewportPos, lastSceneLength }
                  as state
             )
             { initPageNum, mbViewportOffset }
             ( recordTimePosix
             , ((ViewportPos newViewportPosVal) as newViewportPos )
             )

    = let initPageNumVal
              = initPageNum |> fromPageNum

          ( adjustPageNumVal,
            ((ViewportPos syncedViewportPosVal)
                as syncedViewportPos) ) =
              newViewportPos
                  |> adjustViewportPos settings state

          adjustedPageNum
              = initPageNumVal + adjustPageNumVal
              |> boundPageNum settings lastSceneLength

          pageNumVal
              = adjustedPageNum |> fromPageNum |> toFloat

          pageLengthVal
              = virtualPageLength |> fromPageLength

          -- subtract margin size
          virtualViewportPosVal
              = syncedViewportPosVal - wormInMargin

          virtualTotalPageLength
              = pageNumVal * pageLengthVal

          restRealViewPortPosVal
              = lastViewportPos - virtualTotalPageLength

          syncedLastViewportOffset
              = case ( mbViewportOffset
                     , adjustPageNumVal == 0 ) of
                    ( Just viewportOffset, True ) ->
                        viewportOffset
                            |> Debug.log "keep orig"
                    _ ->
                        -- ( Just viewportOffset, False )
                        (restRealViewPortPosVal - virtualViewportPosVal)
                            |> Debug.log "reset"

          mbLastWarpInfo
              = if adjustPageNumVal == 0 then
                    Nothing
                else
                    Just { from
                               = ( initPageNumVal
                                     |> PageNum
                                 , virtualViewportPosVal
                                     |> ViewportPos
                                 )
                         , to
                               = ( adjustedPageNum
                                 , syncedViewportPos
                                 )
                         }
      in { virtualPageNum
               = adjustedPageNum
         , lastViewportPos
             = syncedViewportPos
         , lastClockTime
             = recordTimePosix
               |> Time.posixToMillis
         , lastViewportOffset
             = syncedLastViewportOffset
         , mbLastWarpInfo
             = mbLastWarpInfo
         }


{-| 
-}
updatePosAndRealViewportPos
    : Settings SanityOk ->
      { stateWith |
        lastScrollClock         : Time.Posix
      , lastViewportPos         : Float
      , lastViewportLength      : Float
      , lastSceneLength         : Float
      , scrollStopCheckTime     : Int
      } ->
      ( Time.Posix, ViewportPos SanityUnknown ) ->
      Pos compatibleSanity viewportCompatibleSanity ->
      ( Pos SanityUnknown SanityOk, Float )

updatePosAndRealViewportPos
        ( (Settings {virtualPageLength}) as settings)
        ({ lastScrollClock, scrollStopCheckTime,
           lastViewportLength, lastSceneLength } as state)
        (( newScrollClock, (ViewportPos newViewportPosVal) ) as newPosInfo)
        -- note: `ViewportPos SanityUnknown` will be handled by
        -- `setPosHelper`
        ((Pos lastPosRec) as lastPos)

    = let
        newScrollClockTime
            = newScrollClock
            |> Time.posixToMillis

        lastVirtualPageNumVal
            = lastPosRec.virtualPageNum
            |> fromPageNum
            |> toFloat

        absolutePos
            = calcRealViewportPos settings lastPos

        relativePos
            = newViewportPosVal
              - ( lastPosRec.lastViewportPos
                    |> fromViewportPos
                  )

        isObsolete
            = lastPosRec.lastClockTime + 125 -- 125 ms
              < newScrollClockTime

   in
       let
           ((Pos newPosRec) as newPos)
               = let
                   ( state1, mbViewportOffset )
                       = if absolutePos < 0 then
                             if relativePos <= 0 then
                                 -- keep going bellow the scene
                                 -- : keep hitting the back
                                 ( state
                                 , if isObsolete then
                                       Nothing
                                   else
                                       lastPosRec.lastViewportOffset
                                           - relativePos
                                         |> Just
                                 )
                             else
                                 -- stop hitting the front (decreasing in position)
                                 -- and go backward (increase in position)
                                 -- : no need to change anything if
                                 --   user sync properly
                                 ( state
                                 , Nothing
                                 )
                         else
                             if (absolutePos + lastViewportLength)
                                 > lastSceneLength
                             then
                                 if relativePos >= 0 then
                                     -- keep going beyond the scene
                                     -- : keep hitting the back
                                     ( state
                                     , lastPosRec.lastViewportOffset
                                        - relativePos
                                        |> Just
                                     )

                                 else
                                     -- stop hitting the back
                                     -- (increasing in position)
                                     -- and go forward (decrease in position)
                                     -- : no need to change anything if
                                     --   user sync properly
                                     ( state
                                     , Nothing                               
                                     )

                             else -- on the Scene
                                 if isObsolete  then
                                     ( state{-{ state |
                                         lastViewportPos
                                             = state.lastViewportPos
                                               + relativePos
                                       }-}
                                     , Nothing
                                         |> Debug.log "onScene and obsolete"
                                     )

                                 else -- keep continue scrolling as normal
                                     ( state
                                     , Just <|
                                         lastPosRec.lastViewportOffset
                                     )

              in
                  case mbViewportOffset of
                      Just _ ->
                          newPosInfo
                              |> setPosHelper settings state1
                                 { initPageNum
                                       = lastPosRec.virtualPageNum
                                 , mbViewportOffset
                                       = mbViewportOffset
                                 }
                              |> Pos


                      Nothing ->
                          newPosInfo
                              |> initPos settings state1


       in
           ( newPos
                |> (\x -> Debug.log (calcRealViewportPos settings x |> String.fromFloat) x)
           , newPosRec
                |> sanitizePos
                |> toRealViewportPosVal settings state
           )

{-
{-| make the `Pos` suitable for next `updatePos`
-}
outdatePos : Pos compatibleSanity viewportCompatibleSanity ->
           Pos SanityUnknown viewportCompatibleSanity
outdatePos (Pos posRec)
    = Pos posRec
-}

{-| calculate relative position before being bounded from `0` to `Scene Length`
-}
calcRealViewportPos : Settings SanityOk ->
                      Pos compatibleSanity viewportCompatibleSanity ->
                      Float

calcRealViewportPos (Settings { virtualPageLength, wormInMargin })
                    (Pos pos)
    = let
        { virtualPageNum, lastViewportPos, lastViewportOffset }
            = pos

        pageLength
            = virtualPageLength |> fromPageLength

   in

       ( virtualPageNum |> fromPageNum |> toFloat ) * pageLength
           


       |> (+) ( ( lastViewportPos |> fromViewportPos )
                 - wormInMargin -- remove margin length from Viewport Position
              )
       |> (+) lastViewportOffset


{-| the real viewport value translated from Virtual viewport `Pos`
-}
toRealViewportPosVal
    : Settings SanityOk ->
      { stateWith |
        lastSceneLength : Float
      } ->
      Pos SanityOk viewportCompatibleSanity ->
      Float

toRealViewportPosVal settings { lastSceneLength } pos
    = calcRealViewportPos settings pos
          |> max 0
          |> min lastSceneLength

