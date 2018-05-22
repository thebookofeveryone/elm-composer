module Composer.Text exposing (wrap, trim)

{-| A set of utilities to layout text.

@docs wrap, trim

-}

import Composer.Text.Unit as Unit exposing (Unit)


{-| Wrap text withing a given width.
-}
wrap : Float -> List (Unit inline) -> List (Unit inline)
wrap maxWidth paragraph =
    paragraph
        |> List.foldl
            (\unit ( lineWidth, unitList ) ->
                if unit == Unit.LineBreak then
                    ( 0, Unit.LineBreak :: unitList )
                else
                    let
                        unitWidth =
                            Unit.size unit |> .width
                    in
                        if unitWidth + lineWidth > maxWidth then
                            if lineWidth <= 0 then
                                ( unitWidth, unit :: unitList )
                            else
                                ( unitWidth, unit :: Unit.LineBreak :: unitList )
                        else
                            ( unitWidth + lineWidth, unit :: unitList )
            )
            ( 0, [] )
        |> Tuple.second
        |> List.reverse
        |> trim


{-| Removes spaces at the beginning and the end of each line.
-}
trim : List (Unit inline) -> List (Unit inline)
trim list =
    list
        |> List.foldr
            (\unit ( isNewLine, spaceList, unitList ) ->
                if unit == Unit.LineBreak then
                    ( True, [], Unit.LineBreak :: unitList )
                else if isNewLine then
                    if Unit.isWhitespace unit then
                        ( True, [], unitList )
                    else
                        ( False, [], unit :: unitList )
                else if Unit.isWhitespace unit then
                    ( False, unit :: spaceList, unitList )
                else
                    ( False, [], unit :: List.append spaceList unitList )
            )
            ( True, [], [] )
        |> \( _, _, unitList ) -> unitList
