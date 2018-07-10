module Composer.Primitive
    exposing
        ( Children(Children)
        , Opacity
        , Primitive(Rectangle, Group)
        , origin
        , rect
        , size
        )

{-| A primitive set of geometry units. Composer reduces other complex graphic
languages to these set of geometry primitives. Also, composer backends are feed
with values of this type.

@docs Primitive, Opacity, Children


# Queries

@docs origin, rect, size

-}

import Color exposing (Color)
import Composer.Geometry.Point exposing (Point)
import Composer.Geometry.Rect exposing (Rect)
import Composer.Geometry.Size exposing (Size)
import Composer.Geometry.Transform exposing (Transform)


{-| -}
type alias Opacity =
    Float


{-| -}
type Primitive
    = Rectangle Rect Transform Opacity Color
    | Group Rect Transform Opacity Children


{-| A list of primitives used to avoid type recursion in primitive definition.
-}
type Children
    = Children (List Primitive)


{-| Returns the origin of the given primitive.
-}
origin : Primitive -> Point
origin =
    rect >> .origin


{-| Returns the bounding rectangle of the given primitive.
-}
rect : Primitive -> Rect
rect primitive =
    case primitive of
        Rectangle rect _ _ _ ->
            rect

        Group rect _ _ _ ->
            rect


{-| Returns the size of the given primitive.
-}
size : Primitive -> Size
size =
    rect >> .size
