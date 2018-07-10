module Composer.Geometry
    exposing
        ( BoundingBox
        , Point
        , zeroBoundingBox
        )

{-| Some geometry basics.

@docs BoundingBox, zeroBoundingBox, Point

-}


{-| An axis-aligned bounding box (also known as AABB). A box aligned with
coordinate axes and enclosing some object.
-}
type alias BoundingBox =
    { xMax : Float
    , xMin : Float
    , yMax : Float
    , yMin : Float
    }


{-| An BoundingBox with all the dimensions set to zero.
-}
zeroBoundingBox : BoundingBox
zeroBoundingBox =
    { xMax = 0
    , xMin = 0
    , yMax = 0
    , yMin = 0
    }


{-| A position in the Cartesian coordinate system.
-}
type alias Point =
    { x : Float, y : Float }
