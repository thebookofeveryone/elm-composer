module Helpers.Color exposing (toRgbaString)

import Color exposing (Color)


toRgbaString : Color -> String
toRgbaString color =
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
