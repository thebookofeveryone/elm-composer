module Composer.Scene
    exposing
        ( Scene
        , compose
        )

{-| A `Scene` is set of graphical entities arranged in a spatial representation.
Entities are themselves trees, so a Scene only need to keep the root entity in
order to build complex screens.

A `Scene` also contains the size of the viewport. The viewport is the part
of the scene that is visible. The position is always set to the origin (0, 0).
We can translate the position of the root node to create a camera of a scroll
effect.

The `compose` function translate this scene representation into a flatten set
of primitives that can be consumed by other backend renderers (pdf, svg, canvas,
webgl, etc).

@docs Scene, compose

-}

import Color exposing (Color)
import Composer.Geometry.Size exposing (Size)
import Composer.Geometry.Transform as Transform exposing (Transform)
import Composer.Primitive as Primitive exposing (Primitive)
import Composer.Scene.Entity as Entity exposing (Entity)
import Composer.Scene.Shape as Shape
import Composer.Text as Text
import Composer.Text.Unit as TextUnit


{-| -}
type alias Scene =
    { root : Entity
    , size : Size
    }


{-| Reduce a scene to list of primitives.
-}
compose : Scene -> List Primitive
compose { root } =
    walk
        { transform = Transform.identity
        , opacity = 1
        }
        root


type alias Context =
    { transform : Transform
    , opacity : Float
    }


walk : Context -> Entity -> List Primitive
walk context entity =
    let
        innerContext =
            { transform = transform context entity
            , opacity = opacity context entity
            }

        children =
            entity
                |> Entity.children
                |> List.map (walk innerContext)
                |> List.concat
    in
        List.concat
            [ shape context entity
            , texture context entity
            , text context entity
            , children
            ]


shape : Context -> Entity -> List Primitive
shape context entity =
    case Entity.shape entity of
        Nothing ->
            []

        Just (Shape.Rectangle size) ->
            [ Primitive.Rectangle
                (Entity.identifier entity)
                (transform context entity)
                size
                (color context entity)
            ]


text : Context -> Entity -> List Primitive
text context entity =
    case Entity.text entity of
        Nothing ->
            []

        Just { font, fontSize, text } ->
            let
                t =
                    transform context entity
            in
                text
                    |> TextUnit.fromString font fontSize
                    |> Text.layout (Entity.textLayoutOptions entity)
                    |> List.indexedMap
                        (\index ( point, unit ) ->
                            Primitive.Text
                                (Entity.identifier entity ++ "-" ++ toString index)
                                (Transform.multiply t <| Transform.translate point)
                                (Maybe.withDefault 0 <| TextUnit.fontSize unit)
                                (color context entity)
                                font.name
                                (Maybe.withDefault "" <| TextUnit.text unit)
                        )


texture : Context -> Entity -> List Primitive
texture context entity =
    case Entity.texture entity of
        Nothing ->
            []

        Just ( size, uri ) ->
            [ Primitive.Texture
                (Entity.identifier entity)
                (transform context entity)
                (opacity context entity)
                size
                uri
            ]


transform : Context -> Entity -> Transform
transform context entity =
    Transform.multiply context.transform <| Entity.transform entity


color : Context -> Entity -> Color
color context entity =
    let
        { red, green, blue, alpha } =
            Color.toRgb <| Entity.color entity
    in
        Color.rgba red green blue (context.opacity * Entity.opacity entity * alpha)


opacity : Context -> Entity -> Float
opacity context entity =
    context.opacity * (Entity.opacity entity)
