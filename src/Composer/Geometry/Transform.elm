module Composer.Geometry.Transform
    exposing
        ( Transform
        , apply
        , identity
        , multiply
        , combine
        , translate
        , scale
        , rotation
        )

{-| A 2D linear transform represented by 3x2 matix.

@docs Transform, identity, multiply, combine, apply


# Planar Transformations

@docs translate, scale, rotation

-}

import Composer.Geometry.Point exposing (Point)
import Composer.Geometry.Radian exposing (Radian)


{-| A 2D transform, only meaningfully cells are kept:

    | m11 m21 m31 |
    | m12 m22 m32 | -> (m11, m12, m21, m22, m31, m32) = Transform
    |  0   0   1  |

-}
type alias Transform =
    --  m11    m12    m21    m22    m31    m32
    ( Float, Float, Float, Float, Float, Float )


{-| The identity transform over the ring of transform matrices.
-}
identity : Transform
identity =
    ( 1, 0, 0, 1, 0, 0 )


{-| Computes the matrix product give two transforms.
-}
multiply : Transform -> Transform -> Transform
multiply ( m11, m12, m21, m22, m31, m32 ) ( n11, n12, n21, n22, n31, n32 ) =
    ( m11 * n11 + m21 * n12
    , m12 * n11 + m22 * n12
    , m11 * n21 + m21 * n22
    , m12 * n21 + m22 * n22
    , m11 * n31 + m21 * n32 + m31
    , m12 * n31 + m22 * n32 + m32
    )


{-| Combine multiple transform by multiplying them.
-}
combine : List Transform -> Transform
combine =
    List.foldl multiply identity


{-| Apply a transform to a point.
-}
apply : Transform -> Point -> Point
apply ( m11, m12, m21, m22, m31, m32 ) { x, y } =
    { x = m11 * x + m21 * y + m31
    , y = m12 * x + m22 * y + m32
    }


{-| A Transform that represents a translation to a point.
-}
translate : Point -> Transform
translate { x, y } =
    ( 1, 0, 0, 1, x, y )


{-| A Transform that represents a scaling operation.
-}
scale : Point -> Transform
scale { x, y } =
    ( x, 0, 0, y, 0, 0 )


{-| A Transform that represents a ration of the given radians.
-}
rotation : Radian -> Transform
rotation r =
    let
        c =
            cos r

        s =
            sin r
    in
        ( c, s, s * -1, c, 0, 0 )
