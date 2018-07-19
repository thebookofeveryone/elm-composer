module Composer.Primitive
    exposing
        ( Primitive(Rectangle, Texture)
        )

{-| A primitive set of geometry atoms. Scenes are reduce to a flat list of
primitives by composer. Then, some backends are feed with these primitives to
create pdf, svg, webgl scenes, etc.

@docs Primitive

-}

import Color exposing (Color)
import Composer.Geometry.Size exposing (Size)
import Composer.Geometry.Transform exposing (Transform)


{-| -}
type alias Opacity =
    Float


type alias Id =
    String


{-| -}
type Primitive
    = Rectangle Id Transform Size Color
    | Texture Id Transform Opacity Size String
