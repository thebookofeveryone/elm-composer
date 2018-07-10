module Composer.Geometry
    exposing
        ( BoundingBox
        , zeroBoundingBox
        )

{-| Some geometry basics.

@docs BoundingBox, zeroBoundingBox

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
