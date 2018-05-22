module Composer.Text
    exposing
        ( shrink
        , trim
        , wrap
        )

{-| A set of utilities to layout text.

@docs shrink, trim, wrap

-}

import Composer.Text.Unit as Unit exposing (Unit)
import Composer.Geometry exposing (Size)


{-| Wrap text withing a given width and height. Uses the provided scale factor
to reduce unit sizes between steps, for example a 0.05 value means that the
content will be reduce by steps of 5% until the content fits the required size.

In order to guarantee completeness a maxSteps value is required. Higher values
increase the accuracy of the shrinking. For a 0.05 scaleFactor, 16-32 steps are
enough to layout text with common sizes (from 8 to 120 points).

-}
shrink : { size : Size, scaleFactor : Float, maxSteps : Int } -> List (Unit inline) -> List (Unit inline)
shrink { size, scaleFactor, maxSteps } paragaph =
    let
        wrappedParagraph =
            wrap size.width paragaph

        wrappedParagraphSize =
            Unit.boundingSize wrappedParagraph
    in
        if maxSteps <= 0 then
            wrappedParagraph
        else if wrappedParagraphSize.height > size.height then
            paragaph
                |> List.map (Unit.scale <| 1 - scaleFactor)
                |> shrink
                    { size = size
                    , scaleFactor = scaleFactor
                    , maxSteps = maxSteps - 1
                    }
        else
            wrappedParagraph


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
                            else if Unit.isSingleSpace unit then
                                ( 0, Unit.LineBreak :: unitList )
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
