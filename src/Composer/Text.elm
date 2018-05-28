module Composer.Text
    exposing
        ( HorizontalAlign(Center, Justify, Left, Right)
        , LayoutOptions
        , LineHeight(Absolute, None, Relative)
        , LineHeightMode(Even, Odd)
        , VerticalAlign(Baseline, Bottom, Middle, Top)
        , defaultOptions
        , layout
        , shrink
        , trim
        , wrap
        )

{-| A set of utilities to layout text


# Layout Options

@docs LayoutOptions, defaultOptions, LineHeight, LineHeightMode, HorizontalAlign, VerticalAlign


# Text Layout

@docs layout, shrink, trim, wrap

-}

import Composer.Text.Unit as Unit exposing (Unit)
import Composer.Geometry exposing (Point, Size)


{-| Options used by text layout algorithm:

  - `horizontalAlign` the horizontal alignment of all paragraphs inside the
    bounding box made by the origin and the size.
  - `lineAlign` the vertical alignment of each line.
  - `lineHeight` the height used by each line.
  - `lineHeightMode` the behaviour to follow when choosing a line height for a
    paragraph.
  - `maxSteps` the number of steps that the shrink algorithm can do at most. See
    shrink function for more info.
  - `scaleFactor` the scale applied on each step when shrinking text. See shrink
    function for more info.
  - `size` the size of the bounding box where the text is layout.
  - `verticalAlign` the vertical alignment of all paragraphs inside the
    bounding box made by the origin and the size.

-}
type alias LayoutOptions =
    { horizontalAlign : HorizontalAlign
    , lineAlign : VerticalAlign
    , lineHeight : LineHeight
    , lineHeightMode : LineHeightMode
    , maxSteps : Int
    , scaleFactor : Float
    , size : Size
    , verticalAlign : VerticalAlign
    }


{-| The value used to set the line height of a text. It can be relative to the
default font line height or an absolute value.
-}
type LineHeight
    = Absolute Float
    | None
    | Relative Float


{-| The line height mode is used when computing the line height of a paragraph.
When odd, each line can have different line height from the other lines. When
even, all lines of a paragraph share the same line height.
-}
type LineHeightMode
    = Even
    | Odd


{-| Specifies the horizontal alignment of a paragraph.
-}
type HorizontalAlign
    = Center
    | Justify
    | Left
    | Right


{-| Specifies the vertical alignment of a paragraph or the vertical alignment of
a line. Note that when used to align the whole paragraph, Baseline is ignored.
-}
type VerticalAlign
    = Baseline
    | Bottom
    | Middle
    | Top


{-| Creates a layout options set given a bounding size with sensible defaults.
-}
defaultOptions : Size -> LayoutOptions
defaultOptions size =
    { horizontalAlign = Left
    , lineAlign = Baseline
    , lineHeight = None
    , lineHeightMode = Even
    , maxSteps = 16
    , scaleFactor = 0.05
    , size = size
    , verticalAlign = Top
    }


{-| Layout a paragraph (a list of units) filling the available space and
following the provided layout options. Usually this involves wrapping the text
into the available width and shrink the units scale until it fits the available
height.
-}
layout : LayoutOptions -> List (Unit inline) -> List ( Point, Unit inline )
layout opts paragraph =
    paragraph
        |> shrink { size = opts.size, scaleFactor = opts.scaleFactor, maxSteps = opts.maxSteps }
        |> Unit.joinWords
        |> List.map (\unit -> ( { x = 0, y = 0 }, unit ))


lines : List (Unit inline) -> List ( Size, List (Unit inline) )
lines unitList =
    unitList
        |> List.foldr
            (\unit ( { width, height } as size, line, lineList ) ->
                case unit of
                    Unit.LineBreak ->
                        ( { width = 0, height = 0 }, [], ( size, line ) :: lineList )

                    _ ->
                        let
                            unitSize =
                                Unit.size unit
                        in
                            ( { width = width + unitSize.width
                              , height = max height unitSize.height
                              }
                            , unit :: line
                            , lineList
                            )
            )
            ( { width = 0, height = 0 }, [], [] )
        |> (\( size, line, lineList ) ->
                ( size, line ) :: lineList
           )


{-| Wrap text withing a given width and reduce the scale until it fits in the
provided height. Uses the provided scale factor to reduce unit sizes between
steps, for example a 0.05 value means that the content will be reduce by steps
of 5% until the content fits the required size.

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
