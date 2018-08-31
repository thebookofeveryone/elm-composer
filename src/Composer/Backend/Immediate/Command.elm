module Composer.Backend.Immediate.Command exposing (Command(..), encode)

{-| A command for immediate graphic interfaces. Immediate refers to way these
commands must be interpreted. HTML 2D Canvas is an example of this interface.

@docs Command, encode

-}

import Composer.Geometry.Rect as Geometry
import Composer.Geometry.Transform as Geometry
import Composer.Geometry.Point as Geometry
import Json.Encode as JE exposing (Value)


-- Following conventions in
-- <http://bucephalus.org/text/CanvasHandbook/CanvasHandbook.html>


{-| -}
type Command
    = -- State
      SaveState
    | RestoreState
      -- Transformations
    | Transform Geometry.Transform
      -- Compositing
    | Opacity Float
      -- Colors and Styles
    | FillStyle String
      -- Simple shapes (rectangles)
    | FillRect Geometry.Rect
      -- Text
    | Font String Float
    | FillText String Geometry.Point
      -- Drawing Image
    | DrawImage String Geometry.Rect


{-| Encodes a command into a JSON value.
-}
encode : Command -> Value
encode command =
    case command of
        SaveState ->
            JE.object [ ( "type", JE.string "saveState" ) ]

        RestoreState ->
            JE.object [ ( "type", JE.string "restoreState" ) ]

        Transform transform ->
            JE.object
                [ ( "type", JE.string "transform" )
                , ( "transform", encodeTransform transform )
                ]

        Opacity opacity ->
            JE.object
                [ ( "type", JE.string "opacity" )
                , ( "opacity", JE.float opacity )
                ]

        FillStyle style ->
            JE.object
                [ ( "type", JE.string "fillStyle" )
                , ( "style", JE.string style )
                ]

        FillRect rect ->
            JE.object
                [ ( "type", JE.string "fillRect" )
                , ( "rect", encodeRect rect )
                ]

        Font name size ->
            JE.object
                [ ( "type", JE.string "font" )
                , ( "name", JE.string name )
                , ( "size", JE.float size )
                ]

        FillText text origin ->
            JE.object
                [ ( "type", JE.string "fillText" )
                , ( "text", JE.string text )
                , ( "origin", encodePoint origin )
                ]

        DrawImage src rect ->
            JE.object
                [ ( "type", JE.string "drawImage" )
                , ( "src", JE.string src )
                , ( "rect", encodeRect rect )
                ]


encodeTransform : Geometry.Transform -> Value
encodeTransform ( m11, m12, m21, m22, m31, m32 ) =
    JE.list
        [ JE.float m11
        , JE.float m12
        , JE.float m21
        , JE.float m22
        , JE.float m31
        , JE.float m32
        ]


encodePoint : Geometry.Point -> Value
encodePoint { x, y } =
    JE.list
        [ JE.float x
        , JE.float y
        ]


encodeRect : Geometry.Rect -> Value
encodeRect { origin, size } =
    JE.list
        [ JE.float origin.x
        , JE.float origin.y
        , JE.float size.width
        , JE.float size.height
        ]
