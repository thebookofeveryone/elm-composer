module Composer.Geometry
    exposing
        ( BoundingBox
        , Offset
        , Point
        , Size
        , zeroBoundingBox
        , zeroOffset
        )

{-| Some geometry basics.

@docs BoundingBox, zeroBoundingBox, Offset, zeroOffset, Point, Size

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


{-| A position in the Cartesian coordinate system.
-}
type alias Point =
    { x : Float, y : Float }
