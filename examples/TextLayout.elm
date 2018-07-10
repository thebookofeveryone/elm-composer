module TextLayout exposing (main)

import Composer.BoundingBox as BoundingBox exposing (BoundingBox)
import Composer.Point exposing (Point)
import Composer.Text as Text exposing (HorizontalAlign, LayoutOptions, LineHeight, LineHeightMode, VerticalAlign)
import Composer.Text.Font as Font exposing (Font)
import Composer.Text.Unit as Unit exposing (Unit)
import Fixtures.Cp1252 as Cp1252
import Fixtures.OpenSans as OpenSans
import Helpers.Svg as Helpers
import Html as H exposing (Html)
import Html.Attributes as H
import Html.Events as H
import Svg as S exposing (Svg)
import Svg.Attributes as S


-- Model --


type alias Model =
    { bounds : BoundingBox
    , fontSize : Float
    , horizontalAlign : HorizontalAlign
    , lineHeight : LineHeight
    , lineHeightAbsolute : Float
    , lineHeightMode : LineHeightMode
    , lineHeightRelative : Float
    , text : String
    , verticalAlign : VerticalAlign
    , verticalLineAlign : VerticalAlign
    }


empty : Model
empty =
    { bounds =
        { xMin = 50
        , yMin = 50
        , xMax = 462
        , yMax = 462
        }
    , fontSize = 16
    , horizontalAlign = Text.Left
    , lineHeight = Text.None
    , lineHeightAbsolute = 20
    , lineHeightMode = Text.Even
    , lineHeightRelative = 1
    , text = "Your bones don't break, mine do. That's clear. Your cells react to bacteria and viruses differently than mine. You don't get sick, I do. That's also clear. But for some reason, you and I react the exact same way to water. We swallow it too fast, we choke. We get some in our lungs, we drown.  However unreal it may seem, we are connected, you and I. We're on the same curve, just on opposite ends."
    , verticalAlign = Text.Top
    , verticalLineAlign = Text.Baseline
    }


width : Model -> Float
width { bounds } =
    bounds.xMax - bounds.xMin


height : Model -> Float
height { bounds } =
    bounds.yMax - bounds.yMin


lines : Model -> List { text : String, fontSize : Float, origin : Point }
lines model =
    model
        |> units
        |> Text.layout (options model)
        |> List.map
            (\( point, unit ) ->
                case unit of
                    Unit.Word { text, fontSize } ->
                        { text = text, fontSize = fontSize, origin = point }

                    _ ->
                        { text = "", fontSize = 16, origin = point }
            )


units : Model -> List (Unit inline)
units { text, fontSize } =
    let
        weUnit =
            Unit.fromString Cp1252.codePage OpenSans.font (fontSize * 2) "We "
    in
        text
            |> String.split "We"
            |> List.map (Unit.fromString Cp1252.codePage OpenSans.font fontSize)
            |> List.intersperse weUnit
            |> List.concat


options : Model -> LayoutOptions
options model =
    { horizontalAlign = model.horizontalAlign
    , lineAlign = model.verticalLineAlign
    , lineHeight = model.lineHeight
    , lineHeightMode = model.lineHeightMode
    , maxSteps = 64
    , scaleFactor = 0.05
    , size =
        { width = model.bounds.xMax - model.bounds.xMin
        , height = model.bounds.yMax - model.bounds.yMin
        }
    , verticalAlign = model.verticalAlign
    }


setBoundsX : Float -> Model -> Model
setBoundsX x ({ bounds } as model) =
    { model | bounds = { bounds | xMin = x } }


setBoundsY : Float -> Model -> Model
setBoundsY y ({ bounds } as model) =
    { model | bounds = { bounds | yMin = y } }


setBoundsWidth : Float -> Model -> Model
setBoundsWidth width ({ bounds } as model) =
    { model | bounds = { bounds | xMax = bounds.xMin + width } }


setBoundsHeight : Float -> Model -> Model
setBoundsHeight height ({ bounds } as model) =
    { model | bounds = { bounds | yMax = bounds.yMin + height } }


setFontSize : Float -> Model -> Model
setFontSize fontSize model =
    { model | fontSize = fontSize }


setLineHeight : LineHeight -> Model -> Model
setLineHeight lineHeight model =
    case lineHeight of
        Text.None ->
            { model | lineHeight = Text.None }

        Text.Absolute value ->
            { model | lineHeight = Text.Absolute value, lineHeightAbsolute = value }

        Text.Relative value ->
            { model | lineHeight = Text.Relative value, lineHeightRelative = value }


setLineHeightMode : LineHeightMode -> Model -> Model
setLineHeightMode lineHeightMode model =
    { model | lineHeightMode = lineHeightMode }


setHorizontalAlign : HorizontalAlign -> Model -> Model
setHorizontalAlign horizontalAlign model =
    { model | horizontalAlign = horizontalAlign }


setText : String -> Model -> Model
setText text model =
    { model | text = text }


setVerticalAlign : VerticalAlign -> Model -> Model
setVerticalAlign verticalAlign model =
    { model | verticalAlign = verticalAlign }


setVerticalLineAlign : VerticalAlign -> Model -> Model
setVerticalLineAlign verticalLineAlign model =
    { model | verticalLineAlign = verticalLineAlign }


hasAbsoluteLineHeight : Model -> Bool
hasAbsoluteLineHeight { lineHeight } =
    case lineHeight of
        Text.Absolute _ ->
            True

        _ ->
            False


hasRelativeLineHeight : Model -> Bool
hasRelativeLineHeight { lineHeight } =
    case lineHeight of
        Text.Relative _ ->
            True

        _ ->
            False



-- View --


view : Model -> Html Msg
view model =
    H.div
        [ H.style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "width", "100%" )
            , ( "height", "100%" )
            ]
        ]
        [ H.h1
            [ H.style
                [ ( "width", "100%" )
                , ( "text-align", "center" )
                ]
            ]
            [ H.text "Text Layout"
            ]
        , H.div
            [ H.style
                [ ( "display", "flex" )
                , ( "height", "100%" )
                ]
            ]
            [ H.aside
                [ H.style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "width", "30%" )
                    ]
                ]
                (controls model)
            , H.main_
                [ H.style
                    [ ( "display", "flex" )
                    , ( "align-items", "center" )
                    , ( "justify-content", "center" )
                    , ( "flex-grow", "1" )
                    ]
                ]
                [ canvas model
                ]
            ]
        ]


controls : Model -> List (Html Msg)
controls model =
    [ H.h2 [] [ H.text "Bounds" ]
    , H.label []
        [ H.text "X"
        , H.input
            [ H.style [ ( "margin", "0 0 0 8px" ), ( "width", "80%" ) ]
            , H.type_ "range"
            , H.max "512"
            , H.min "0"
            , H.value <| toString model.bounds.xMin
            , H.onInput OnBoundsXChange
            ]
            []
        ]
    , H.label []
        [ H.text "Y"
        , H.input
            [ H.style [ ( "margin", "0 0 0 8px" ), ( "width", "80%" ) ]
            , H.type_ "range"
            , H.max "512"
            , H.min "0"
            , H.value <| toString model.bounds.yMin
            , H.onInput OnBoundsYChange
            ]
            []
        ]
    , H.label []
        [ H.text "Width"
        , H.input
            [ H.style [ ( "margin", "0 0 0 8px" ), ( "width", "80%" ) ]
            , H.type_ "range"
            , H.max "512"
            , H.min "0"
            , H.value <| toString <| width model
            , H.onInput OnBoundsWidthChange
            ]
            []
        ]
    , H.label []
        [ H.text "Height"
        , H.input
            [ H.style [ ( "margin", "0 0 0 8px" ), ( "width", "80%" ) ]
            , H.type_ "range"
            , H.max "512"
            , H.min "0"
            , H.value <| toString <| height model
            , H.onInput OnBoundsHeightChange
            ]
            []
        ]
    , H.h2 [] [ H.text "Text" ]
    , H.label []
        [ H.text "Font Size"
        , H.input
            [ H.style [ ( "margin", "0 0 0 8px" ), ( "width", "80%" ) ]
            , H.type_ "range"
            , H.max "512"
            , H.min "4"
            , H.value <| toString model.fontSize
            , H.onInput OnFontSizeChange
            ]
            []
        ]
    , H.textarea
        [ H.style [ ( "margin", "12px 0 0 0px" ) ]
        , H.attribute "rows" "8"
        , H.value model.text
        , H.onInput OnTextChange
        ]
        []
    , H.h2 [] [ H.text "Layout" ]
    , H.h3 [] [ H.text "Horizontal Align" ]
    , H.div []
        [ H.input
            [ H.type_ "radio"
            , H.id "horizontalalign-left"
            , H.name "horizontalalign"
            , H.checked <| model.horizontalAlign == Text.Left
            , H.onClick <| OnHorizontalAlignChange Text.Left
            ]
            []
        , H.label [ H.for "horizontalalign-left" ] [ H.text "Left" ]
        , H.input
            [ H.type_ "radio"
            , H.id "horizontalalign-right"
            , H.name "horizontalalign"
            , H.checked <| model.horizontalAlign == Text.Right
            , H.onClick <| OnHorizontalAlignChange Text.Right
            ]
            []
        , H.label [ H.for "horizontalalign-right" ] [ H.text "Right" ]
        , H.input
            [ H.type_ "radio"
            , H.id "horizontalalign-center"
            , H.name "horizontalalign"
            , H.checked <| model.horizontalAlign == Text.Center
            , H.onClick <| OnHorizontalAlignChange Text.Center
            ]
            []
        , H.label [ H.for "horizontalalign-center" ] [ H.text "Center" ]
        , H.input
            [ H.type_ "radio"
            , H.id "horizontalalign-justify"
            , H.name "horizontalalign"
            , H.checked <| model.horizontalAlign == Text.Justify
            , H.onClick <| OnHorizontalAlignChange Text.Justify
            ]
            []
        , H.label [ H.for "horizontalalign-justify" ] [ H.text "Justify" ]
        ]
    , H.h3 [] [ H.text "Line Height" ]
    , H.div []
        [ H.input
            [ H.type_ "radio"
            , H.id "lineheight-none"
            , H.name "lineheight"
            , H.checked <| model.lineHeight == Text.None
            , H.onClick <| OnLineHeightChange Text.None
            ]
            []
        , H.label [ H.for "lineheight-none" ] [ H.text "None" ]
        , H.input
            [ H.type_ "radio"
            , H.id "lineheight-absolute"
            , H.name "lineheight"
            , H.checked <| hasAbsoluteLineHeight model
            , H.onClick <| OnLineHeightChange <| Text.Absolute model.lineHeightAbsolute
            ]
            []
        , H.label [ H.for "lineheight-absolute" ] [ H.text "Absolute" ]
        , H.input
            [ H.type_ "number"
            , H.style [ ( "width", "60px" ) ]
            , H.min "0"
            , H.max "512"
            , H.step "1"
            , H.value <| toString model.lineHeightAbsolute
            , H.disabled <| not <| hasAbsoluteLineHeight model
            , H.onInput (String.toFloat >> Result.withDefault 1 >> Text.Absolute >> OnLineHeightChange)
            ]
            []
        , H.input
            [ H.type_ "radio"
            , H.id "lineheight-relative"
            , H.name
                "lineheight"
            , H.checked <| hasRelativeLineHeight model
            , H.onClick <| OnLineHeightChange <| Text.Relative model.lineHeightRelative
            ]
            []
        , H.label [ H.for "lineheight-relative" ] [ H.text "Relative" ]
        , H.input
            [ H.type_ "number"
            , H.style [ ( "width", "60px" ) ]
            , H.min "0"
            , H.max "512"
            , H.step "0.1"
            , H.value <| toString model.lineHeightRelative
            , H.disabled <| not <| hasRelativeLineHeight model
            , H.onInput (String.toFloat >> Result.withDefault 1 >> Text.Relative >> OnLineHeightChange)
            ]
            []
        ]
    , H.h3 [] [ H.text "Line Height Mode" ]
    , H.div []
        [ H.input
            [ H.type_ "radio"
            , H.id "lineheightmode-even"
            , H.name "lineheightmode"
            , H.checked <| model.lineHeightMode == Text.Even
            , H.onClick <| OnLineHeightModeChange Text.Even
            ]
            []
        , H.label [ H.for "lineheightmode-even" ] [ H.text "Even" ]
        , H.input
            [ H.type_ "radio"
            , H.id "lineheightmode-odd"
            , H.name "lineheightmode"
            , H.checked <| model.lineHeightMode == Text.Odd
            , H.onClick <| OnLineHeightModeChange Text.Odd
            ]
            []
        , H.label [ H.for "lineheightmode-odd" ] [ H.text "Odd" ]
        ]
    , H.h3 [] [ H.text "Vertical Align" ]
    , H.div []
        [ H.input
            [ H.type_ "radio"
            , H.id "verticalalign-top"
            , H.name "verticalalign"
            , H.checked <| model.verticalAlign == Text.Top
            , H.onClick <| OnVerticalAlignChange Text.Top
            ]
            []
        , H.label [ H.for "verticalalign-top" ] [ H.text "Top" ]
        , H.input
            [ H.type_ "radio"
            , H.id "verticalalign-middle"
            , H.name "verticalalign"
            , H.checked <| model.verticalAlign == Text.Middle
            , H.onClick <| OnVerticalAlignChange Text.Middle
            ]
            []
        , H.label [ H.for "verticalalign-middle" ] [ H.text "Middle" ]
        , H.input
            [ H.type_ "radio"
            , H.id "verticalalign-bottom"
            , H.name "verticalalign"
            , H.checked <| model.verticalAlign == Text.Bottom
            , H.onClick <| OnVerticalAlignChange Text.Bottom
            ]
            []
        , H.label [ H.for "verticalalign-bottom" ] [ H.text "Bottom" ]
        ]
    , H.h3 [] [ H.text "Vertical Line Align" ]
    , H.div []
        [ H.input
            [ H.type_ "radio"
            , H.id "verticallinealign-top"
            , H.name "verticallinealign"
            , H.checked <| model.verticalLineAlign == Text.Top
            , H.onClick <| OnVerticalLineAlignChange Text.Top
            ]
            []
        , H.label [ H.for "verticallinealign-top" ] [ H.text "Top" ]
        , H.input
            [ H.type_ "radio"
            , H.id "verticallinealign-middle"
            , H.name "verticallinealign"
            , H.checked <| model.verticalLineAlign == Text.Middle
            , H.onClick <| OnVerticalLineAlignChange Text.Middle
            ]
            []
        , H.label [ H.for "verticallinealign-middle" ] [ H.text "Middle" ]
        , H.input
            [ H.type_ "radio"
            , H.id "verticallinealign-baseline"
            , H.name "verticallinealign"
            , H.checked <| model.verticalLineAlign == Text.Baseline
            , H.onClick <| OnVerticalLineAlignChange Text.Baseline
            ]
            []
        , H.label [ H.for "verticallinealign-baseline" ] [ H.text "Baseline" ]
        , H.input
            [ H.type_ "radio"
            , H.id "verticallinealign-bottom"
            , H.name "verticallinealign"
            , H.checked <| model.verticalLineAlign == Text.Bottom
            , H.onClick <| OnVerticalLineAlignChange Text.Bottom
            ]
            []
        , H.label [ H.for "verticallinealign-bottom" ] [ H.text "Bottom" ]
        ]
    ]


canvas : Model -> Svg Msg
canvas model =
    S.svg
        [ S.viewBox "0 0 512 512"
        , S.width "512px"
        , S.height "512px"
        ]
        [ S.defs []
            [ S.node "style"
                [ S.type_ "text/css" ]
                [ H.text """
                            @font-face {
                              font-family: "Open Sans";
                              src: url(https://res.cloudinary.com/thebookofeveryone/raw/upload/v1523352324/fonts/opensans-regular-webfont.ttf);
                            }
                          """
                ]
            ]
        , Helpers.rect
            { x = 1
            , y = 1
            , width = 510
            , height = 510
            , color = "green"
            }
        , Helpers.rect
            { x = model.bounds.xMin
            , y = model.bounds.yMin
            , width = width model
            , height = height model
            , color = "teal"
            }
        , S.g []
            (List.map
                (\{ text, fontSize, origin } ->
                    S.text_
                        [ S.x <| toString <| origin.x + model.bounds.xMin
                        , S.y <| toString <| origin.y + model.bounds.yMin
                        , S.fontSize <| toString fontSize ++ "px"
                        , S.fontFamily "Open Sans"
                        , S.fill "black"
                        ]
                        [ S.text text
                        ]
                )
                (lines model)
            )
        ]



-- View / Parameters --


descent : Float -> Float
descent fontSize =
    toPxUnits fontSize font.description.descent


font : Font
font =
    OpenSans.font


lineHeight : Float -> Float
lineHeight fontSize =
    toPxUnits fontSize
        (font.description.boundingBox.yMax - font.description.boundingBox.yMin)


toPxUnits : Float -> Float -> Float
toPxUnits fontSize units =
    units / 1000 * fontSize



-- Update --


type Msg
    = OnBoundsXChange String
    | OnBoundsHeightChange String
    | OnBoundsWidthChange String
    | OnBoundsYChange String
    | OnFontSizeChange String
    | OnHorizontalAlignChange HorizontalAlign
    | OnLineHeightChange LineHeight
    | OnLineHeightModeChange LineHeightMode
    | OnTextChange String
    | OnVerticalAlignChange VerticalAlign
    | OnVerticalLineAlignChange VerticalAlign


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnBoundsXChange stringValue ->
            stringValue
                |> String.toFloat
                |> Result.withDefault model.bounds.xMin
                |> (\v -> setBoundsX v model)

        OnBoundsYChange stringValue ->
            stringValue
                |> String.toFloat
                |> Result.withDefault model.bounds.yMin
                |> (\v -> setBoundsY v model)

        OnBoundsWidthChange stringValue ->
            stringValue
                |> String.toFloat
                |> Result.withDefault (width model)
                |> (\v -> setBoundsWidth v model)

        OnBoundsHeightChange stringValue ->
            stringValue
                |> String.toFloat
                |> Result.withDefault (height model)
                |> (\v -> setBoundsHeight v model)

        OnLineHeightModeChange lineHeightMode ->
            setLineHeightMode lineHeightMode model

        OnFontSizeChange stringValue ->
            stringValue
                |> String.toFloat
                |> Result.withDefault 16
                |> (\v -> setFontSize v model)

        OnHorizontalAlignChange horizontalAlign ->
            setHorizontalAlign horizontalAlign model

        OnLineHeightChange lineHeight ->
            setLineHeight lineHeight model

        OnTextChange text ->
            setText text model

        OnVerticalAlignChange verticalAlign ->
            setVerticalAlign verticalAlign model

        OnVerticalLineAlignChange verticalLineAlign ->
            setVerticalLineAlign verticalLineAlign model



-- Initialization --


main : Program Never Model Msg
main =
    H.beginnerProgram
        { model = empty
        , view = view
        , update = update
        }
