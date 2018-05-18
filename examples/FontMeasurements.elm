module FontMeasurements exposing (main)

import Composer.Text.Font as Font exposing (Font)
import Fixtures.Cp1252 as Cp1252
import Fixtures.OpenSans as OpenSans
import Helpers.Svg as Helpers
import Html as H exposing (Html)
import Html.Attributes as H
import Html.Events as H
import Svg as S exposing (Svg)
import Svg.Attributes as S


-- View --


view : ( String, Int ) -> Html Msg
view ( text, fontSize ) =
    H.div
        [ H.style
            [ ( "text-align", "center" )
            ]
        ]
        [ H.main_
            [ H.style
                [ ( "display", "inline-block" )
                , ( "width", "1024px" )
                ]
            ]
            [ H.h1 [] [ H.text "Font Measurements" ]
            , H.label []
                [ H.text "Text"
                , H.input
                    [ H.style [ ( "margin", "0 32px 32px 8px" ) ]
                    , H.value text
                    , H.onInput OnTextChange
                    ]
                    []
                ]
            , H.label []
                [ H.text "Font Size"
                , H.input
                    [ H.style [ ( "margin", "0 0 0 8px" ) ]
                    , H.type_ "range"
                    , H.max "256"
                    , H.min "16"
                    , H.value <| toString fontSize
                    , H.onInput OnFontSizeChange
                    ]
                    []
                ]
            , S.svg
                [ S.viewBox "0 0 1024 256"
                , S.width "1024px"
                , S.height "256px"
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
                    { x = 0.5
                    , y = 0.5
                    , width = 1023.5
                    , height = 255.5
                    , color = "green"
                    }
                , Helpers.rect
                    { x = ox fontSize text
                    , y = oy fontSize
                    , width = textWidth fontSize text
                    , height = lineHeight fontSize
                    , color = "blue"
                    }
                , S.text_
                    [ S.x <| toString <| ox fontSize text
                    , S.y <| toString <| oy fontSize + lineHeight fontSize + descent fontSize
                    , S.fontSize <| toString fontSize ++ "px"
                    , S.fontFamily "Open Sans"
                    , S.fill "black"
                    ]
                    [ S.text text
                    ]
                , Helpers.line
                    { x = ox fontSize text
                    , y = oy fontSize + lineHeight fontSize + descent fontSize
                    }
                    { x = ox fontSize text + textWidth fontSize text
                    , y = oy fontSize + lineHeight fontSize + descent fontSize
                    }
                    { color = "green", size = max 1 (toFloat fontSize / 50) }
                ]
            ]
        ]



-- View / Parameters --


textWidth : Int -> String -> Float
textWidth fontSize text =
    toPxUnits fontSize <| Font.stringWidth Cp1252.codePage font text


ox : Int -> String -> Float
ox fontSize text =
    1024 / 2 - textWidth fontSize text / 2


oy : Int -> Float
oy fontSize =
    256 / 2 - lineHeight fontSize / 2


lineHeight : Int -> Float
lineHeight fontSize =
    toPxUnits fontSize
        (font.description.boundingBox.yMax - font.description.boundingBox.yMin)


ascent : Int -> Float
ascent fontSize =
    toPxUnits fontSize font.description.ascent


descent : Int -> Float
descent fontSize =
    toPxUnits fontSize font.description.descent



-- View / Constants --


font : Font
font =
    OpenSans.font



-- View / Svg Helpers --


toPxUnits : Int -> Float -> Float
toPxUnits fontSize units =
    units / 1000 * (toFloat fontSize)



-- Update


type Msg
    = OnFontSizeChange String
    | OnTextChange String


update : Msg -> ( String, Int ) -> ( String, Int )
update msg model =
    case msg of
        OnFontSizeChange stringValue ->
            case String.toInt stringValue of
                Ok value ->
                    ( Tuple.first model, value )

                Err _ ->
                    model

        OnTextChange text ->
            ( text, Tuple.second model )



-- Main and Initialization --


main : Program Never ( String, Int ) Msg
main =
    H.beginnerProgram
        { model = ( "Elm Composer", 128 )
        , view = view
        , update = update
        }
