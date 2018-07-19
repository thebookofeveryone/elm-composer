module EntitiesSvg exposing (main)

import Color
import Composer.Backend.Svg as Svg
import Composer.Geometry.Transform as Transform
import Composer.Scene as Scene exposing (Scene)
import Composer.Scene.Entity as Entity
import Composer.Scene.Shape as Shape
import Html as H exposing (Html)
import Html.Attributes as H


main : Html msg
main =
    H.div
        []
        (H.h1
            [ H.style [ ( "width", "100%" ), ( "text-align", "center" ) ]
            ]
            [ H.text "Examples Using SVG Backend" ]
            :: List.map unitView
                [ basics

                --, layout
                , transformsAndCompositing
                , textures
                ]
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


type alias Unit =
    { title : String
    , cases : List Case
    }


type alias Case =
    { title : String
    , scene : Scene
    }


basics : Unit
basics =
    { title = "Basic Examples"
    , cases =
        [ { title = "A green rect"
          , scene =
                { size = { width = 512, height = 512 }
                , root =
                    Entity.empty "rect"
                        |> Entity.setShape (Just <| Shape.Rectangle { width = 512, height = 512 })
                        |> Entity.setColor Color.green
                }
          }
        , { title = "A green rect inside a blue rect"
          , scene =
                { size = { width = 512, height = 512 }
                , root =
                    Entity.empty "parent"
                        |> Entity.setShape (Just <| Shape.Rectangle { width = 512, height = 512 })
                        |> Entity.setColor Color.blue
                        |> Entity.addChild
                            (Entity.empty "child"
                                |> Entity.setShape (Just <| Shape.Rectangle { width = 256, height = 256 })
                                |> Entity.setColor Color.green
                                |> Entity.setTransform (Transform.translate { x = 128, y = 128 })
                            )
                }
          }
        ]
    }


layout : Unit
layout =
    { title = "Layout"
    , cases =
        [ { title = "Percent size / percent position"
          , scene =
                { size = { width = 512, height = 512 }
                , root = Entity.empty "root"
                }
          }
        , { title = "content size"
          , scene =
                { size = { width = 512, height = 512 }
                , root = Entity.empty "root"
                }
          }
        , { title = "relative position"
          , scene =
                { size = { width = 512, height = 512 }
                , root = Entity.empty "root"
                }
          }
        ]
    }


transformsAndCompositing : Unit
transformsAndCompositing =
    { title = "Transforms And Compositing"
    , cases =
        [ { title = "A translucid rect"
          , scene =
                { size = { width = 512, height = 512 }
                , root =
                    Entity.empty "parent"
                        |> Entity.setShape (Just <| Shape.Rectangle { width = 512, height = 512 })
                        |> Entity.setColor Color.blue
                        |> Entity.setChildren
                            (List.range 0 3
                                |> List.map toFloat
                                |> List.map
                                    (\j ->
                                        List.range 0 3
                                            |> List.map toFloat
                                            |> List.map
                                                (\i ->
                                                    Entity.empty ("rect-" ++ toString i)
                                                        |> Entity.setShape (Just <| Shape.Rectangle { width = 100, height = 100 })
                                                        |> Entity.setColor Color.green
                                                        |> Entity.setTransform (Transform.translate { x = 22.4 * (i + 1) + 100 * i, y = 22.4 * (j + 1) + 100 * j })
                                                        |> Entity.setOpacity (0.25 * j + 0.0625 * i)
                                                )
                                    )
                                |> List.concat
                            )
                }
          }
        , { title = "A rotated rect"
          , scene =
                { size = { width = 512, height = 512 }
                , root =
                    Entity.empty "parent"
                        |> Entity.setShape (Just <| Shape.Rectangle { width = 512, height = 512 })
                        |> Entity.setColor Color.blue
                        |> Entity.setChildren
                            (List.range 0 3
                                |> List.map toFloat
                                |> List.map
                                    (\j ->
                                        List.range 0 3
                                            |> List.map toFloat
                                            |> List.map
                                                (\i ->
                                                    Entity.empty ("rect-" ++ toString i)
                                                        |> Entity.setShape (Just <| Shape.Rectangle { width = 100, height = 100 })
                                                        |> Entity.setColor Color.green
                                                        |> Entity.setTransform
                                                            (Transform.combine
                                                                [ Transform.translate { x = -50, y = -50 }
                                                                , Transform.rotation <| (j * 4 + i) * 5.75 * pi / 180
                                                                , Transform.translate { x = 22.4 * (i + 1) + 100 * i + 50, y = 22.4 * (j + 1) + 100 * j + 50 }
                                                                ]
                                                            )
                                                        |> Entity.setOpacity 0.65
                                                )
                                    )
                                |> List.concat
                            )
                }
          }
        , { title = "A rotated group of entities"
          , scene =
                { size = { width = 512, height = 512 }
                , root =
                    Entity.empty "parent"
                        |> Entity.setShape (Just <| Shape.Rectangle { width = 512, height = 512 })
                        |> Entity.setColor Color.blue
                        |> Entity.setChildren
                            [ Entity.empty "first-background-rect"
                                |> Entity.setShape (Just <| Shape.Rectangle { width = 256, height = 256 })
                                |> Entity.setColor Color.darkBlue
                            , Entity.empty "second-background-rect"
                                |> Entity.setShape (Just <| Shape.Rectangle { width = 256, height = 256 })
                                |> Entity.setColor Color.yellow
                                |> Entity.setTransform (Transform.translate { x = 256, y = 256 })
                            , Entity.empty "container"
                                |> Entity.setShape (Just <| Shape.Rectangle { width = 256, height = 256 })
                                |> Entity.setColor Color.green
                                |> Entity.setTransform
                                    (Transform.combine
                                        [ Transform.translate { x = -128, y = -128 }
                                        , Transform.rotation <| 45 * pi / 180
                                        , Transform.translate { x = 256, y = 256 }
                                        ]
                                    )
                                |> Entity.setChildren
                                    [ Entity.empty "top-foreground-rect"
                                        |> Entity.setShape (Just <| Shape.Rectangle { width = 128, height = 128 })
                                        |> Entity.setColor Color.darkGreen
                                    , Entity.empty "bottom-foreground-rect"
                                        |> Entity.setShape (Just <| Shape.Rectangle { width = 128, height = 128 })
                                        |> Entity.setColor Color.darkGreen
                                        |> Entity.setTransform (Transform.translate { x = 128, y = 128 })
                                    , Entity.empty "left-foreground-rect"
                                        |> Entity.setShape (Just <| Shape.Rectangle { width = 64, height = 64 })
                                        |> Entity.setColor Color.darkGreen
                                        |> Entity.setTransform
                                            (Transform.combine
                                                [ Transform.translate { x = -32, y = -32 }
                                                , Transform.rotation <| 45 * pi / 180
                                                , Transform.translate { x = 64, y = 192 }
                                                ]
                                            )
                                    , Entity.empty "right-foreground-rect"
                                        |> Entity.setShape (Just <| Shape.Rectangle { width = 64, height = 64 })
                                        |> Entity.setColor Color.darkGreen
                                        |> Entity.setTransform
                                            (Transform.combine
                                                [ Transform.translate { x = -32, y = -32 }
                                                , Transform.rotation <| 45 * pi / 180
                                                , Transform.translate { x = 192, y = 64 }
                                                ]
                                            )
                                    ]
                            ]
                }
          }
        ]
    }


textures : Unit
textures =
    { title = "Textures"
    , cases =
        [ { title = "An awesome texture"
          , scene =
                { size = { width = 512, height = 512 }
                , root =
                    Entity.empty "parent"
                        |> Entity.setShape (Just <| Shape.Rectangle { width = 512, height = 512 })
                        |> Entity.setColor Color.blue
                        |> Entity.addChild
                            (Entity.empty "child"
                                |> Entity.setTransform (Transform.translate { x = 106, y = 145 })
                                |> Entity.setTexture
                                    (Just <|
                                        ( { width = 300, height = 222 }
                                        , "https://upload.wikimedia.org/wikipedia/en/thumb/5/5f/Original_Doge_meme.jpg/300px-Original_Doge_meme.jpg"
                                        )
                                    )
                            )
                }
          }
        , { title = "Wow, very transformed"
          , scene =
                { size = { width = 512, height = 512 }
                , root =
                    Entity.empty "parent"
                        |> Entity.setShape (Just <| Shape.Rectangle { width = 512, height = 512 })
                        |> Entity.setColor Color.blue
                        |> Entity.addChild
                            (Entity.empty "child"
                                |> Entity.setTransform
                                    (Transform.combine
                                        [ Transform.translate { x = -150, y = -111 }
                                        , Transform.rotation <| 45 * pi / 180
                                        , Transform.translate { x = 256, y = 256 }
                                        ]
                                    )
                                |> Entity.setOpacity 0.75
                                |> Entity.setTexture
                                    (Just <|
                                        ( { width = 300, height = 222 }
                                        , "https://upload.wikimedia.org/wikipedia/en/thumb/5/5f/Original_Doge_meme.jpg/300px-Original_Doge_meme.jpg"
                                        )
                                    )
                            )
                }
          }
        ]
    }
