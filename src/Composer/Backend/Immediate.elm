module Composer.Backend.Immediate exposing (render)

{-| Converts a list of primitives into a list of immediate commands. Immediate
commands are sort of an imperative graphic interface, used by 2D canvas for
example.

@docs render

-}

import Composer.Backend.Immediate.Command as Command exposing (Command)
import Helpers.Color as Color
import Composer.Primitive as Primitive exposing (Primitive)
import Composer.Backend.Immediate.Command as Command


{-| -}
render : List Primitive -> List Command
render primitiveList =
    List.concat <| List.map renderPrimitive primitiveList


renderPrimitive : Primitive -> List Command
renderPrimitive primitive =
    case primitive of
        Primitive.Rectangle id t size color ->
            [ Command.SaveState
            , Command.Transform t
            , Command.FillStyle <| Color.toRgbaString color
            , Command.FillRect { origin = { x = 0, y = 0 }, size = size }
            , Command.RestoreState
            ]

        Primitive.Text id t fontSize color fontName text ->
            [ Command.SaveState
            , Command.Transform t
            , Command.Font fontName fontSize
            , Command.FillStyle <| Color.toRgbaString color
            , Command.FillText text { x = 0, y = 0 }
            , Command.RestoreState
            ]

        Primitive.Texture id t opacity size uri ->
            [ Command.SaveState
            , Command.Transform t
            , Command.Opacity opacity
            , Command.DrawImage uri { origin = { x = 0, y = 0 }, size = size }
            , Command.RestoreState
            ]
