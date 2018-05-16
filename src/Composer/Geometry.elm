module Composer.Geometry exposing (BoundingBox, Offset, Size)

{-| Some geometry basics.

@docs BoundingBox, Offset, Size

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


{-| A set of four dimensions: top, left, bottom and right. Used for offsets,
padding, margins, etc.
-}
type alias Offset =
    { top : Float
    , left : Float
    , bottom : Float
    , right : Float
    }


{-| A width and height pair.
-}
type alias Size =
    { width : Float
    , height : Float
    }
