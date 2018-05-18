module Composer.Geometry exposing (BoundingBox, Offset, Size, zeroOffset)

{-| Some geometry basics.

@docs BoundingBox, Offset, zeroOffset, Size

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


{-| An offset with all the dimensions set to zero.
-}
zeroOffset : Offset
zeroOffset =
    { top = 0
    , left = 0
    , bottom = 0
    , right = 0
    }


{-| A width and height pair.
-}
type alias Size =
    { width : Float
    , height : Float
    }
