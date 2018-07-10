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

import Composer.Text.Unit as Unit exposing (Unit, Metrics)
import Composer.Geometry as Geometry exposing (Point, Size)
import Composer.Offset as Offset exposing (Offset)


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
        |> layoutHorizontally opts
        |> layoutVertically opts
        |> List.concat


joinWordsIfNeeded : LayoutOptions -> List (Unit inline) -> List (Unit inline)
joinWordsIfNeeded { horizontalAlign } units =
    if horizontalAlign == Justify then
        units
    else
        Unit.joinWords units


attachSizes : List (List (Unit inline)) -> List ( Metrics, List ( Size, Unit inline ) )
attachSizes =
    List.map <|
        List.foldr
            (\unit ( { size, offset }, acc ) ->
                let
                    unitSize =
                        Unit.size unit

                    unitOffset =
                        Unit.offset unit
                in
                    ( { size =
                            { width = size.width + unitSize.width
                            , height = max size.height unitSize.height
                            }
                      , offset =
                            if unitSize.height > size.height then
                                unitOffset
                            else
                                offset
                      }
                    , ( unitSize, unit ) :: acc
                    )
            )
            ( { size = { width = 0, height = 0 }, offset = Offset.zero }, [] )


layoutHorizontally : LayoutOptions -> List ( Metrics, List ( Size, Unit inline ) ) -> List ( Metrics, List ( Point, Unit inline ) )
layoutHorizontally { size, horizontalAlign } list =
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
                    (\( metrics, line ) ->
                        ( metrics
                        , spread (size.width - metrics.size.width) line
                        )
                    )
                    list

            Center ->
                List.map
                    (\( metrics, line ) ->
                        ( metrics
                        , spread ((size.width - metrics.size.width) / 2) line
                        )
                    )
                    list

            Justify ->
                let
                    shiftedList =
                        List.append (Maybe.withDefault [] <| List.tail list)
                            [ ( { size = { width = 0, height = 0 }
                                , offset = Offset.zero
                                }
                              , []
                              )
                            ]
                in
                    shiftedList
                        |> List.map2 (,) list
                        |> List.map
                            (\( ( metrics, line ), ( _, nextLine ) ) ->
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
                                        ( metrics, spread 0 line )
                                    else
                                        ( { size = { width = lineWithoutSpacesWidth, height = metrics.size.height }
                                          , offset = metrics.offset
                                          }
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


layoutVertically : LayoutOptions -> List ( Metrics, List ( Point, Unit inline ) ) -> List (List ( Point, Unit inline ))
layoutVertically opts lineList =
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
            |> harmonizeMetricsIfNeeded opts
            |> List.foldl
                (\( metrics, line ) { yOffset, lineAcc } ->
                    let
                        lineHeight =
                            applyLineHeight opts.lineHeight metrics.size.height
                    in
                        { yOffset = yOffset + lineHeight
                        , lineAcc =
                            List.map
                                (\( point, unit ) ->
                                    let
                                        unitVerticalOffset =
                                            lineOffset opts metrics lineHeight unitOffset unit + unitOffset.top

                                        unitOffset =
                                            Unit.offset unit
                                    in
                                        ( { x = point.x
                                          , y = yOffset + lineHeight + baseOffset + unitVerticalOffset
                                          }
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


harmonizeMetricsIfNeeded : LayoutOptions -> List ( Metrics, any ) -> List ( Metrics, any )
harmonizeMetricsIfNeeded { lineHeightMode } lineList =
    case lineHeightMode of
        Odd ->
            lineList

        Even ->
            let
                dominant =
                    lineList
                        |> List.map Tuple.first
                        |> dominantMetrics
            in
                List.map (Tuple.mapFirst <| always dominant) lineList


lineOffset : LayoutOptions -> Metrics -> Float -> Offset -> Unit inline -> Float
lineOffset { lineAlign } lineMetrics lineHeight unitOffset unit =
    case lineAlign of
        Bottom ->
            0

        Baseline ->
            lineMetrics.offset.top - unitOffset.top

        Middle ->
            let
                { height } =
                    Unit.size unit
            in
                -(lineHeight - height) / 2

        Top ->
            let
                { height } =
                    Unit.size unit
            in
                -(lineHeight - height)


dominantMetrics : List Metrics -> Metrics
dominantMetrics =
    List.foldl
        (\metrics dominant ->
            if metrics.size.height > dominant.size.height then
                metrics
            else
                dominant
        )
        { size = { width = 0, height = 0 }, offset = Offset.zero }


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
