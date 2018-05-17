module Helpers exposing (char)

import Char
import Expect as E
import Fuzz as F exposing (Fuzzer)
import Helpers.Char as Char
import Test as T exposing (Test)


char : Test
char =
    T.describe "Char Helpers"
        [ T.describe "isWhitespace"
            [ T.test "Detect common whitespace characters" <|
                E.all
                    [ always <| E.true "space" <| Char.isWhitespace ' '
                    , always <| E.true "line feed" <| Char.isWhitespace '\n'
                    , always <| E.true "tab" <| Char.isWhitespace '\t'
                    ]
            , T.fuzz alphaNumCharFuzzer
                "Avoids common non whitespace characters"
                (Char.isWhitespace >> E.false "not a whitespace")
            ]
        ]


alphaNumCharFuzzer : Fuzzer Char
alphaNumCharFuzzer =
    [ F.intRange (Char.toCode 'A') (Char.toCode 'Z')
    , F.intRange (Char.toCode 'a') (Char.toCode 'z')
    , F.intRange (Char.toCode '0') (Char.toCode '9')
    , F.constant (Char.toCode '_')
    ]
        |> F.oneOf
        |> F.map Char.fromCode
