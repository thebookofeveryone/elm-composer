module Composer.Backend.Svg exposing (render)

{-| Converts a list of primitive in a svg node ready to use as part of an HTML
document.

@docs render

-}

import Color exposing (Color)
import Composer.Geometry.Size exposing (Size)
import Composer.Geometry.Transform exposing (Transform)
import Composer.Primitive as Primitive exposing (Primitive)
import Svg as S exposing (Svg, Attribute)
import Svg.Attributes as S


{-| -}
render : Size -> List Primitive -> Svg msg
render size primitiveList =
    S.svg [ viewBox size ] <|
        List.map renderPrimitive primitiveList


renderPrimitive : Primitive -> Svg msg
renderPrimitive primitive =
    case primitive of
        Primitive.Rectangle id t { width, height } color ->
            S.rect
                [ S.id id
                , S.x "0"
                , S.y "0"
                , transform t
                , S.width <| toString width
                , S.height <| toString height
                , fillColor color
                ]
                []

        Primitive.Texture id t opacity { width, height } uri ->
            S.image
                [ S.id id
                , S.x "0"
                , S.y "0"
                , transform t
                , S.opacity <| toString opacity
                , S.width <| toString width
                , S.height <| toString height
                , S.xlinkHref uri
                ]
                []



-- Svg Attributes Helpers --


viewBox : Size -> Attribute msg
viewBox { width, height } =
    S.viewBox <| "0 0 " ++ toString width ++ " " ++ toString height


transform : Transform -> Attribute msg
transform t =
    S.transform <| matrixString t


fillColor : Color -> Attribute msg
fillColor color =
    S.fill <| rgbaColorString color



-- Misc Helpers --


rgbaColorString : Color -> String
rgbaColorString color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        "rgba("
            ++ toString red
            ++ ","
            ++ toString green
            ++ ","
            ++ toString blue
            ++ ","
            ++ toString alpha
            ++ ")"


matrixString : Transform -> String
matrixString ( m11, m12, m21, m22, m31, m32 ) =
    "matrix("
        ++ toString m11
        ++ ","
        ++ toString m12
        ++ ","
        ++ toString m21
        ++ ","
        ++ toString m22
        ++ ","
        ++ toString m31
        ++ ","
        ++ toString m32
        ++ ")"