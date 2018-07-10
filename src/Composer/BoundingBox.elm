module Composer.BoundingBox
    exposing
        ( BoundingBox
        , zero
        )

{-| A BoundingBox is a rectangular box in 2D defined by a minimum and maximum X
and Y values.

@docs BoundingBox, zero

-}


type alias BoundingBox =
    { xMax : Float
    , xMin : Float
    , yMax : Float
    , yMin : Float
    }


{-| An BoundingBox with all the dimensions set to zero.
-}
zero : BoundingBox
zero =
    { xMax = 0
    , xMin = 0
    , yMax = 0
    , yMin = 0
    }
