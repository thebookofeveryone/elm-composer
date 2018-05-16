module Text exposing (font, codePage)

import Array
import Composer.Text.Font as Font
import Composer.Text.Font.CodePage as CodePage
import Expect as E
import Fixtures.Cp1252 as Cp1252
import Fixtures.OpenSans as OpenSans
import Helpers.Result as Result
import Test as T exposing (Test)


codePage : Test
codePage =
    T.describe "CodePage"
        [ T.test "decodes a valid definition" <|
            \() ->
                Cp1252.codePage
                    |> E.all
                        [ CodePage.codepoint '?' >> E.equal (Just 0x3F)
                        , CodePage.codepoint 'a' >> E.equal (Just 0x61)
                        , CodePage.index 0x61 >> E.equal (Just 'a')
                        ]
        ]


font : Test
font =
    T.describe "Font"
        [ T.test "decodes a valid json" <|
            \() ->
                OpenSans.font
                    |> E.all
                        [ .description >> .ascent >> E.equal 1048
                        , .description >> .boundingBox >> .xMax >> E.equal 960
                        , .description >> .boundingBox >> .xMin >> E.equal -191
                        , .description >> .boundingBox >> .yMax >> E.equal 931
                        , .description >> .boundingBox >> .yMin >> E.equal -242
                        , .description >> .capHeight >> E.equal 714
                        , .description >> .descent >> E.equal -291
                        , .description >> .italicAngle >> E.equal 0.01
                        , .description >> .missingWidth >> E.equal 530
                        , .widths >> Array.get 32 >> Maybe.withDefault 0 >> E.equal 260
                        , .name >> E.equal "Open Sans"
                        , .type_ >> E.equal Font.TrueType
                        ]
        , T.describe "glyphWidth"
            [ T.test "returns a known glyph width" <|
                \() ->
                    'a'
                        |> Font.glyphWidth Cp1252.codePage OpenSans.font
                        |> E.equal 556
            , T.test "fallback to default width if an unknown glyph is provided" <|
                \() ->
                    '💩'
                        |> Font.glyphWidth Cp1252.codePage OpenSans.font
                        |> E.equal (.missingWidth <| .description <| OpenSans.font)
            ]
        ]
