module Composer.Geometry.Point exposing (Point, origin)

{-| A type to describe positions in the Cartesian coordinate system.

@docs Point, origin

-}


{-| -}
type alias Point =
    { x : Float, y : Float }


{-| The origin (0, 0).
-}
origin : Point
origin =
    { x = 0, y = 0 }
