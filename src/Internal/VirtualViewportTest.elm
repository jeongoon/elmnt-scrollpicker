module Internal.VirtualViewportTest
    exposing (..)

import Internal.VirtualViewport as V

import Time

initState -- of Acutal viewport you want to use it for dislay
    = { lastScrollClock
            = Time.millisToPosix 1000
      , lastViewportPos = 450
      , lastSceneLength = 1000
      , scrollStopCheckTime = 75
      }

settings
    = V.toSettings
      { virtualPageLength
            = V.makePageLength 600
      , wormOutPadding
            = 150
      , wormInMargin
            = 100
      }

initViewportPosVal
    = 300 -- in the middle

relativeScrollingList : List Int
relativeScrollingList
    = (List.repeat 22 25) -- total 500 px : (upto 20th) 800 -> (worm in) 825 -> 850
      ++
      [ -50 -- trying to go up -> 
      , 50  -- go down again   ->
      , -100 -- go up again ->
      ]
      ++
      (List.repeat 20 -50) -- keep going up
      ++
      [ 25 -- go down exactly 25 from the top
      , -50 -- go top again
      , 10  -- go down exactly 10 from the top again.
      ]

timeAndRelativeScrollingList
    = relativeScrollingList
    |> List.indexedMap
       (\idx rscroll ->
            ( Time.millisToPosix <| 1000 + idx * 10
            , rscroll
            )
       )
run =
    timeAndRelativeScrollingList
        |> List.foldl
           (\(t, rscroll) acc ->
                let
                    ( vpos, pos, state )
                        = case acc |> List.head of
                              Just ( { virtualPos }
                                   , state_
                                   ) ->
                                      
                                  let vpos0
                                          = virtualPos
                                          |> V.fromPos
                                          |> .lastViewportPos

                                      vpos_
                                          = vpos0
                                          |> V.fromViewportPos
                                          |> (+) ( rscroll |> toFloat )
                                          |> V.makeViewportPos
                                  in
                                      ( vpos_
                                      , virtualPos
                                          |> V.updatePos settings state_
                                             (t, vpos_)
                                      , state_
                                      )
                              _ ->
                                  let vpos_
                                          = V.makeViewportPos initViewportPosVal
                                  in
                                      ( vpos_
                                      , ( t, vpos_ )
                                           |> V.initPos settings initState
                                      , initState
                                      )
                    realPos
                        = pos
                        |> V.toRealViewportPos settings state

                    log
                        =
                          Debug.log
                              (  "\nrcroll: " ++ ( rscroll |> String.fromInt ) ++ "\n" ++
                                 "vpos  : " ++ ( vpos
                                                   |> V.fromViewportPos
                                                   |> String.fromFloat)      ++ "\n" ++
                                 "real  : " ++ ( realPos |> String.fromFloat )
                              ) "."

                in
                    ( { relativeScroll
                            = rscroll
                      , virtualPos
                            = pos
                      , realPos
                            = realPos
                      }
                    , { state |
                        lastViewportPos
                            = realPos
                      , lastScrollClock
                            = t
                      }
                      ) :: acc
           ) []


                
                  
