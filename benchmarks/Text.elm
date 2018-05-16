module Text exposing (main)

import Benchmark as B exposing (Benchmark)
import Benchmark.Runner as B exposing (BenchmarkProgram)
import Composer.Text.Font as Font exposing (Font)
import Fixtures.Cp1252 as Cp1252
import Fixtures.OpenSans as OpenSans


suite : Benchmark
suite =
    B.describe "Text"
        [ B.describe "Font"
            [ B.benchmark "stringWidth" <|
                \() ->
                    Font.stringWidth Cp1252.codePage OpenSans.font samuelIpsum
            ]
        ]


main : BenchmarkProgram
main =
    B.program suite


samuelIpsum : String
samuelIpsum =
    """
    Your bones don't break, mine do. That's clear. Your cells react to bacteria
    and viruses differently than mine. You don't get sick, I do. That's also
    clear. But for some reason, you and I react the exact same way to water. We
    swallow it too fast, we choke. We get some in our lungs, we drown. However
    unreal it may seem, we are connected, you and I. We're on the same curve,
    just on opposite ends.
    """
