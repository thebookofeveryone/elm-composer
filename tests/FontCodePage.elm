module FontCodePage exposing (suite)

import Composer.Text.Font.CodePage as CodePage exposing (CodePage)
import Expect as E
import Helpers.Result as Result
import Test as T exposing (Test)


suite : Test
suite =
    T.describe "Composer.Text.Font.Encoding Decoder"
        [ T.test "decodes a valid file" <|
            \() ->
                """
                  !1F U+001F .notdef
                  !3F U+003F question
                  !40 U+0040 at
                  !41 U+0041 A
                  !42 U+0042 B
                  !43 U+0043 C
                  !61 U+0061 a
                  !62 U+0062 b
                  !63 U+0063 c
                """
                    |> CodePage.fromString
                    |> E.all
                        [ mapWithEmpty >> CodePage.codepoint '?' >> E.equal (Just 0x3F)
                        , mapWithEmpty >> CodePage.codepoint 'a' >> E.equal (Just 0x61)
                        , mapWithEmpty >> CodePage.index 0x61 >> E.equal (Just 'a')
                        ]
        ]


mapWithEmpty : Result err CodePage -> CodePage
mapWithEmpty =
    Result.mapWithDefault CodePage.empty identity
