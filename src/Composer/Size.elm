module Composer.Size exposing (Size, zero)

{-| A type to describe 2d sizes.

@docs Size, zero

-}


{-| A type containing width and height.
-}
type alias Size =
    { width : Float
    , height : Float
    }


{-| A size with all the dimensions set to zero.
-}
zero : Size
zero =
    { width = 0
    , height = 0
    }
