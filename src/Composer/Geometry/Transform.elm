module Composer.Geometry.Transform
    exposing
        ( Transform
        , identity
        , multiply
        , translate
        , rotation
        , combine
        , rotationFromCenter
        )

{-| A 2D linear transform represented by 3x2 matix.

@docs Transform , identity , multiply , translate , rotation , combine , rotationFromCenter

-}

import Composer.Geometry.Point exposing (Point)
import Composer.Geometry.Radian exposing (Radian)
import Composer.Geometry.Size exposing (Size)


{-| -}
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


{-| A Transform that represents a translation to a point.
-}
translate : Point -> Transform
translate { x, y } =
    ( 1, 0, 0, 1, x, y )


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


{-| Combines multiple transforms applying matrix product in reverse order.
-}
combine : List Transform -> Transform
combine =
    List.foldr (\m a -> multiply a m) identity


{-| A transform that applies a rotating in the center of a rectangle.
-}
rotationFromCenter : Point -> Size -> Radian -> Transform
rotationFromCenter { x, y } { width, height } angle =
    let
        w2 =
            x + width * 0.5

        h2 =
            y + height * 0.5
    in
        combine
            [ translate { x = -w2, y = -h2 }
            , rotation angle
            , translate { x = w2, y = -h2 }
            ]
