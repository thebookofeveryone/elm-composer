module TextLayout exposing (main)

import Composer.Geometry as Geometry exposing (BoundingBox)
import Helpers.Svg as Helpers
import Html as H exposing (Html)
import Html.Attributes as H
import Html.Events as H
import Svg as S exposing (Svg)
import Svg.Attributes as S


-- Model --


type alias Model =
    { bounds : BoundingBox }


empty : Model
empty =
    { bounds =
        { xMin = 50
        , yMin = 50
        , xMax = 462
        , yMax = 462
        }
    }


width : Model -> Float
width { bounds } =
    bounds.xMax - bounds.xMin


height : Model -> Float
height { bounds } =
    bounds.yMax - bounds.yMin


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
            [ H.style [ ( "margin", "0 0 0 8px" ) ]
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
            [ H.style [ ( "margin", "0 0 0 8px" ) ]
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
            [ H.style [ ( "margin", "0 0 0 8px" ) ]
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
            [ H.style [ ( "margin", "0 0 0 8px" ) ]
            , H.type_ "range"
            , H.max "512"
            , H.min "0"
            , H.value <| toString <| height model
            , H.onInput OnBoundsHeightChange
            ]
            []
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
        ]



-- Update --


type Msg
    = OnBoundsXChange String
    | OnBoundsYChange String
    | OnBoundsWidthChange String
    | OnBoundsHeightChange String


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



-- Initialization --


main : Program Never Model Msg
main =
    H.beginnerProgram
        { model = empty
        , view = view
        , update = update
        }
