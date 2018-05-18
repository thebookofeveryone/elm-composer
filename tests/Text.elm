module Text exposing (font, codePage, unit)

import Array
import Composer.Geometry as Geometry
import Composer.Text.Font as Font
import Composer.Text.Font.CodePage as CodePage
import Composer.Text.Unit as Unit
import Expect as E
import Fixtures.Cp1252 as Cp1252
import Fixtures.OpenSans as OpenSans
import Fuzz as F
import Test as T exposing (Test)


codePage : Test
codePage =
    T.describe "CodePage"
        [ T.test "decodes a valid definition" <|
            \() ->
                Cp1252.codePage
                    |> E.all
                        [ CodePage.index '?' >> E.equal (Just 0x3F)
                        , CodePage.index 'a' >> E.equal (Just 0x61)
                        , CodePage.char 0x61 >> E.equal (Just 'a')
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
        , T.describe "kerning"
            [ T.test "returns a known glyph pair distance" <|
                \() ->
                    'e'
                        |> Font.kerning Cp1252.codePage OpenSans.font 'T'
                        |> E.equal -70
            ]
        , T.describe "stringWidth"
            [ T.test "returns a known string width" <|
                \() ->
                    "foobar"
                        |> Font.stringWidth Cp1252.codePage OpenSans.font
                        |> E.equal 3124
            , T.test "returns zero when the string is empty" <|
                \() ->
                    ""
                        |> Font.stringWidth Cp1252.codePage OpenSans.font
                        |> E.equal 0
            ]
        ]


unit : Test
unit =
    T.describe "Text.Unit"
        [ T.describe "size"
            [ T.test "return the proper size of a well known Word Unit" <|
                \() ->
                    "elm-composer"
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> List.head
                        |> Maybe.andThen List.head
                        |> Maybe.map Unit.size
                        |> Maybe.withDefault { width = 0, height = 0 }
                        |> E.equal { width = 107.824, height = 18.768 }
            , T.fuzz (F.tuple3 ( F.float, F.float, F.float )) "return the proper size of any Inline unit" <|
                \( width, height, scale ) ->
                    ()
                        |> Unit.embed scale { width = width, height = height } Geometry.zeroOffset
                        |> Unit.size
                        |> E.equal
                            { width = width * scale
                            , height = height * scale
                            }
            ]
        , T.describe "fromString"
            [ T.test "returns an empty list when an empty string is provided" <|
                \() ->
                    ""
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> E.equal [ [] ]
            , T.test "manages a well known string" <|
                \() ->
                    " To the  Moon   "
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> List.head
                        |> Maybe.withDefault []
                        |> List.map (Unit.text >> Maybe.withDefault "")
                        |> E.equal [ " ", "To", " ", "the", "  ", "Moon", "   " ]
            , T.fuzz (F.intRange 1 9999) "keeps spaces at the beginning of the string" <|
                \spacesLength ->
                    (String.repeat spacesLength " " ++ "foobar")
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> E.all
                            [ List.length >> E.equal 1
                            , List.head
                                >> Maybe.withDefault []
                                >> List.length
                                >> E.equal 2
                            , List.head
                                >> Maybe.withDefault []
                                >> List.head
                                >> Maybe.andThen Unit.text
                                >> Maybe.withDefault ""
                                >> String.length
                                >> E.equal spacesLength
                            ]
            , T.fuzz (F.intRange 1 9999) "keeps spaces at the end of the string" <|
                \spacesLength ->
                    ("foobar" ++ String.repeat spacesLength " ")
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> E.all
                            [ List.length >> E.equal 1
                            , List.head
                                >> Maybe.withDefault []
                                >> List.length
                                >> E.equal 2
                            , List.head
                                >> Maybe.withDefault []
                                >> List.reverse
                                >> List.head
                                >> Maybe.andThen Unit.text
                                >> Maybe.withDefault ""
                                >> String.length
                                >> E.equal spacesLength
                            ]
            ]
        ]
