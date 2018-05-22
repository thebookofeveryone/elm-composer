module TextLayout exposing (main)

import Composer.Geometry as Geometry exposing (BoundingBox)
import Composer.Text as Text
import Composer.Text.Font as Font exposing (Font)
import Composer.Text.Unit as Unit
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
    , text : String
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
    , text = "Your bones don't break, mine do. That's clear. Your cells react to bacteria and viruses differently than mine. You don't get sick, I do. That's also clear. But for some reason, you and I react the exact same way to water. We swallow it too fast, we choke. We get some in our lungs, we drown.  However unreal it may seem, we are connected, you and I. We're on the same curve, just on opposite ends."
    }


width : Model -> Float
width { bounds } =
    bounds.xMax - bounds.xMin


height : Model -> Float
height { bounds } =
    bounds.yMax - bounds.yMin


lines : Model -> ( Float, List String )
lines ({ text, fontSize } as model) =
    text
        |> Unit.fromString Cp1252.codePage OpenSans.font fontSize
        |> Text.shrink
            { size = { width = width model, height = height model }
            , scaleFactor = 0.1
            , maxSteps = 32
            }
        |> (\list ->
                ( case List.head list of
                    Just (Unit.Word { fontSize }) ->
                        fontSize

                    _ ->
                        16
                , list
                    |> Unit.joinWords
                    |> Unit.toParagraph
                    |> List.map (String.join " ")
                )
           )


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


setText : String -> Model -> Model
setText text model =
    { model | text = text }



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
            (let
                ( fontSize, list ) =
                    lines model
             in
                List.indexedMap
                    (\index line ->
                        S.text_
                            [ S.x <| toString model.bounds.xMin
                            , S.y <| toString <| model.bounds.yMin + (toFloat (index + 1) * lineHeight fontSize)
                            , S.fontSize <| toString fontSize ++ "px"
                            , S.fontFamily "Open Sans"
                            , S.fill "black"
                            ]
                            [ S.text line
                            ]
                    )
                    list
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
    | OnTextChange String


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

        OnFontSizeChange stringValue ->
            stringValue
                |> String.toFloat
                |> Result.withDefault 16
                |> (\v -> setFontSize v model)

        OnTextChange text ->
            setText text model



-- Initialization --


main : Program Never Model Msg
main =
    H.beginnerProgram
        { model = empty
        , view = view
        , update = update
        }
