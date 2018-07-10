module Composer.Offset exposing (Offset, zero)

{-| A type useful for define paddings, margins, etc.

@docs Offset, zero

-}


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
zero : Offset
zero =
    { top = 0
    , left = 0
    , bottom = 0
    , right = 0
    }
