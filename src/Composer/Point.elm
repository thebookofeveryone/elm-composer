module Composer.Point exposing (Point, origin)

{-| A type to describe 2d points.

@docs Point, origin

-}


{-| A position in the Cartesian coordinate system.
-}
type alias Point =
    { x : Float, y : Float }


{-| The origin (0, 0).
-}
origin : Point
origin =
    { x = 0, y = 0 }
