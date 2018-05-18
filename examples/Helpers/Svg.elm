module Helpers.Svg exposing (rect, line)

import Svg as S exposing (Svg)
import Svg.Attributes as S


rect : { x : Float, y : Float, width : Float, height : Float, color : String } -> Svg msg
rect { x, y, width, height, color } =
    S.rect
        [ S.fill "none"
        , S.stroke color
        , S.strokeWidth "1"
        , S.width <| toString width
        , S.height <| toString height
        , S.x <| toString x
        , S.y <| toString y
        ]
        []


line : { x : Float, y : Float } -> { x : Float, y : Float } -> { color : String, size : Float } -> Svg msg
line p1 p2 { color, size } =
    S.line
        [ S.x1 <| toString p1.x
        , S.y1 <| toString p1.y
        , S.x2 <| toString p2.x
        , S.y2 <| toString p2.y
        , S.stroke color
        , S.strokeWidth <| toString size
        ]
        []
