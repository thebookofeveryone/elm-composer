module Helpers.Unit exposing (fuzzer, lineStats)

import Composer.Size exposing (Size)
import Composer.Text.Font exposing (Font)
import Composer.Text.Font.CodePage exposing (CodePage)
import Composer.Text.Unit as Unit exposing (Unit)
import Fuzz as F exposing (Fuzzer)


fuzzer : CodePage -> Font -> Float -> Fuzzer (Unit inline)
fuzzer codePage font fontSize =
    F.frequency
        [ ( 1, F.constant Unit.LineBreak )
        , ( 9
          , F.string
                |> F.conditional
                    { retries = 2
                    , fallback = always "."
                    , condition = String.length >> ((<) 0)
                    }
                |> F.map
                    (\text ->
                        Unit.Word
                            { codePage = codePage
                            , font = font
                            , fontSize = fontSize
                            , text = text
                            }
                    )
          )
        ]


lineStats : List (Unit inline) -> List { size : Size, count : Int }
lineStats =
    List.foldr
        (\unit ( stats, list ) ->
            if unit == Unit.LineBreak then
                ( { size = { width = 0, height = 0 }, count = 0 }, stats :: [] )
            else
                let
                    { width, height } =
                        Unit.size unit
                in
                    ( { size =
                            { width = width + stats.size.width
                            , height = max height stats.size.height
                            }
                      , count = stats.count + 1
                      }
                    , list
                    )
        )
        ( { size = { width = 0, height = 0 }, count = 0 }, [] )
        >> (\( stats, list ) -> stats :: list)
