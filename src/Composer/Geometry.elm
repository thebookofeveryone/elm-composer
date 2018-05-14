module Composer.Geometry exposing (BoundingBox, Size)

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


{-| A width and height pair.
-}
type alias Size =
    { width : Float
    , height : Float
    }
