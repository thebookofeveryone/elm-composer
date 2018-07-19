module Composer.Scene.Shape exposing (Shape(Rectangle))

{-| A set a geometrical shapes that entities can draw.

@docs Shape

-}

import Composer.Geometry.Size exposing (Size)


{-| -}
type Shape
    = Rectangle Size
