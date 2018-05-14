module Composer.Geometry exposing (BoundingBox)

{-| Some geometry basics.

@docs BoundingBox

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
