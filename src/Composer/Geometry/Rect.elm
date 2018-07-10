module Composer.Geometry.Rect
    exposing
        ( Rect
        , atOrigin
        )

{-| A 2D rectangle defined by a point and a size.

@docs Rect , atOrigin

-}

import Composer.Geometry.Point as Point exposing (Point)
import Composer.Geometry.Size exposing (Size)


{-| -}
type alias Rect =
    { origin : Point
    , size : Size
    }


{-| A rectangle laying on the origin of coordinates (0, 0).
-}
atOrigin : Size -> Rect
atOrigin size =
    { origin = Point.origin
    , size = size
    }
