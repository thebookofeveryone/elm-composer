module Composer.Primitive
    exposing
        ( Primitive(Rectangle, Text, Texture)
        , Opacity
        , FontName
        , FontSize
        , Id
        , ImageUri
        )

{-| A primitive set of geometry atoms. Scenes are reduce to a flat list of
primitives by composer. Then, some backends are feed with these primitives to
create pdf, svg, webgl scenes, etc.

@docs Primitive


## Type Aliases

@docs Opacity, Id, ImageUri, FontName, FontSize

-}

import Color exposing (Color)
import Composer.Geometry.Size exposing (Size)
import Composer.Geometry.Transform exposing (Transform)
import Composer.Text.Font exposing (Font)


{-| -}
type Primitive
    = Rectangle Id Transform Size Color
    | Text Id Transform FontSize Color FontName String
    | Texture Id Transform Opacity Size ImageUri


{-| A float value, 0 means transparent, 1 opaque.
-}
type alias Opacity =
    Float


{-| A string identifier.
-}
type alias Id =
    String


{-| A URI pointing to an image.
-}
type alias ImageUri =
    String


{-| The font size of a text.
-}
type alias FontSize =
    Float


{-| The font name of a text.
-}
type alias FontName =
    String
