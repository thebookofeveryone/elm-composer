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
        |> shrink opts
        |> joinWordsIfNeeded opts
        |> Unit.lines
        |> attachSizes
        |> horizontalLayout opts
        |> verticalLayout opts
        |> List.concat


joinWordsIfNeeded : LayoutOptions -> List (Unit inline) -> List (Unit inline)
joinWordsIfNeeded { horizontalAlign } units =
    if horizontalAlign == Justify then
        units
    else
        Unit.joinWords units


attachSizes : List (List (Unit inline)) -> List ( Size, List ( Size, Unit inline ) )
attachSizes =
    List.map <|
        List.foldr
            (\unit ( size, acc ) ->
                let
                    unitSize =
                        Unit.size unit
                in
                    ( { width = size.width + unitSize.width
                      , height = max size.height unitSize.height
                      }
                    , ( unitSize, unit ) :: acc
                    )
            )
            ( { width = 0, height = 0 }, [] )


horizontalLayout : LayoutOptions -> List ( Size, List ( Size, Unit inline ) ) -> List ( Size, List ( Point, Unit inline ) )
horizontalLayout { size, horizontalAlign } list =
    let
        spread diff line =
            line
                |> List.foldl
                    (\( unitSize, unit ) { xOffset, acc } ->
                        { xOffset = xOffset + unitSize.width
                        , acc = ( { x = xOffset + diff, y = 0 }, unit ) :: acc
                        }
                    )
                    { xOffset = 0, acc = [] }
                |> .acc
    in
        case horizontalAlign of
            Left ->
                List.map (Tuple.mapSecond <| spread 0) list

            Right ->
                List.map
                    (\( lineSize, line ) ->
                        ( lineSize
                        , spread (size.width - lineSize.width) line
                        )
                    )
                    list

            Center ->
                List.map
                    (\( lineSize, line ) ->
                        ( lineSize
                        , spread ((size.width - lineSize.width) / 2) line
                        )
                    )
                    list

            Justify ->
                let
                    shiftedList =
                        List.append (Maybe.withDefault [] <| List.tail list) [ ( { width = 0, height = 0 }, [] ) ]
                in
                    shiftedList
                        |> List.map2 (,) list
                        |> List.map
                            (\( ( lineSize, line ), ( _, nextLine ) ) ->
                                let
                                    lineWithoutSpaces =
                                        List.filter (not << Unit.isWhitespace << Tuple.second) line

                                    nextLineWithoutSpaces =
                                        List.filter (not << Unit.isWhitespace << Tuple.second) nextLine

                                    lineWithoutSpacesWidth =
                                        lineWithoutSpaces
                                            |> List.map (Tuple.first >> .width)
                                            |> List.sum

                                    spaceWidth =
                                        (size.width - lineWithoutSpacesWidth) / toFloat (List.length lineWithoutSpaces - 1)
                                in
                                    if List.isEmpty nextLineWithoutSpaces then
                                        ( lineSize, spread 0 line )
                                    else
                                        ( { width = lineWithoutSpacesWidth, height = lineSize.height }
                                        , lineWithoutSpaces
                                            |> List.foldl
                                                (\( unitSize, unit ) { xOffset, acc } ->
                                                    { xOffset = xOffset + unitSize.width + spaceWidth
                                                    , acc = ( { x = xOffset, y = 0 }, unit ) :: acc
                                                    }
                                                )
                                                { xOffset = 0, acc = [] }
                                            |> .acc
                                        )
                            )


verticalLayout : LayoutOptions -> List ( Size, List ( Point, Unit inline ) ) -> List (List ( Point, Unit inline ))
verticalLayout opts lineList =
    let
        baseOffset =
            case opts.verticalAlign of
                Bottom ->
                    opts.size.height - height

                Middle ->
                    (opts.size.height - height) / 2

                _ ->
                    0

        height =
            lineList
                |> List.map Tuple.second
                |> List.map (List.map Tuple.second)
                |> Unit.join
                |> heightAfterLayout opts
    in
        lineList
            |> List.foldl
                (\( lineSize, line ) { yOffset, lineAcc } ->
                    let
                        lineHeight =
                            applyLineHeight opts.lineHeight lineSize.height
                    in
                        { yOffset = yOffset + lineHeight
                        , lineAcc =
                            List.map
                                (\( point, unit ) ->
                                    ( { x = point.x, y = yOffset + lineHeight + baseOffset }
                                    , unit
                                    )
                                )
                                line
                                :: lineAcc
                        }
                )
                { yOffset = 0, lineAcc = [] }
            |> .lineAcc
            |> List.reverse


{-| Wrap text withing a given width and reduce the scale until it fits in the
provided height. Uses the provided scale factor to reduce unit sizes between
steps, for example a 0.05 value means that the content will be reduce by steps
of 5% until the content fits the required size.

In order to guarantee completeness a maxSteps value is required. Higher values
increase the accuracy of the shrinking. For a 0.05 scaleFactor, 16-32 steps are
enough to layout text with common sizes (from 8 to 120 points).

-}
shrink : LayoutOptions -> List (Unit inline) -> List (Unit inline)
shrink ({ size, scaleFactor, maxSteps } as opts) paragaph =
    let
        wrappedParagraph =
            wrap size.width paragaph
    in
        if maxSteps <= 0 then
            wrappedParagraph
        else if heightAfterLayout opts wrappedParagraph > size.height then
            paragaph
                |> List.map (Unit.scale <| 1 - scaleFactor)
                |> shrink { opts | maxSteps = maxSteps - 1 }
        else
            wrappedParagraph


heightAfterLayout : LayoutOptions -> List (Unit inline) -> Float
heightAfterLayout { lineHeight, lineHeightMode } unitList =
    let
        reduceHeight { height } acc =
            applyLineHeight lineHeight height + acc

        harmonizeIfNeeded sizeList =
            case lineHeightMode of
                Odd ->
                    sizeList

                Even ->
                    sizeList
                        |> List.foldl (\{ height } acc -> max height acc) 0
                        |> \height ->
                            { width = 0, height = height }
                                |> List.repeat (List.length sizeList)
    in
        unitList
            |> Unit.lines
            |> List.map (Unit.metrics >> .size)
            |> harmonizeIfNeeded
            |> List.foldl reduceHeight 0


applyLineHeight : LineHeight -> Float -> Float
applyLineHeight lineHeight height =
    case lineHeight of
        None ->
            height

        Absolute value ->
            value

        Relative scale ->
            scale * height


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
