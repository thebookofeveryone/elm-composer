module EntitiesSvg exposing (main)

import Composer.Backend.Svg as Svg
import Composer.Scene as Scene exposing (Scene)
import Html as H exposing (Html)
import Html.Attributes as H
import Helpers.Entities as Entities exposing (Unit, Case)


main : Html msg
main =
    H.div
        []
        (H.h1
            [ H.style [ ( "width", "100%" ), ( "text-align", "center" ) ]
            ]
            [ H.text "Examples Using SVG Backend" ]
            :: List.map unitView Entities.all
        )


unitView : Unit -> Html msg
unitView { title, cases } =
    H.section
        [ H.style
            [ ( "display", "flex" )
            , ( "flex-wrap", "wrap" )
            , ( "justify-content", "space-around" )
            ]
        ]
        (H.h2 [ H.style [ ( "width", "100%" ), ( "text-align", "center" ) ] ] [ H.text title ]
            :: List.map caseView cases
        )


caseView : Case -> Html msg
caseView { title, scene } =
    H.div [ H.style [ ( "width", "512px" ) ] ]
        [ H.h3 [ H.style [ ( "width", "100%" ), ( "text-align", "center" ) ] ] [ H.text title ]
        , H.div
            [ H.style
                [ ( "width", "512px" )
                , ( "height", "512px" )
                , ( "border", "1px solid black" )
                ]
            ]
            [ Svg.render scene.size <| Scene.compose scene ]
        ]
