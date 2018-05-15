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
                        [ .description >> .ascent >> E.equal 765
                        , .description >> .boundingBox >> .xMax >> E.equal 1204
                        , .description >> .boundingBox >> .xMin >> E.equal -550
                        , .description >> .boundingBox >> .yMax >> E.equal 1048
                        , .description >> .boundingBox >> .yMin >> E.equal -271
                        , .description >> .capHeight >> E.equal 714
                        , .description >> .descent >> E.equal -240
                        , .description >> .italicAngle >> E.equal 0.01
                        , .description >> .missingWidth >> E.equal 600
                        , .widths >> Array.get 32 >> Maybe.withDefault 0 >> E.equal 260
                        , .name >> E.equal "OpenSans"
                        , .type_ >> E.equal Font.TrueType
                        ]
        , T.describe "glyphWidth"
            [ T.test "returns a known glyph width" <|
                \() ->
                    OpenSans.font
                        |> Font.glyphWidth 'a' Cp1252.codePage
                        |> E.equal 556
            , T.test "fallback to default width if an unknown glyph is provided" <|
                \() ->
                    OpenSans.font
                        |> Font.glyphWidth '💩' Cp1252.codePage
                        |> E.equal 600
            ]
        ]
