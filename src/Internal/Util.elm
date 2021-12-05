module Internal.Util
    exposing (..)


import Element

flip f a b = f b a

curry f a b = f (a, b)

uncurry f (a, b) = f a b

dupAsPairs n = ( n , n )

sizeScaled = Element.modular 16 1.25 >> round
