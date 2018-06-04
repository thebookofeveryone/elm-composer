module Text
    exposing
        ( codePage
        , font
        , text
        , unit
        )

import Array
import Composer.Geometry as Geometry
import Composer.Text as Text
import Composer.Text.Font as Font
import Composer.Text.Font.CodePage as CodePage
import Composer.Text.Unit as Unit
import Expect as E
import Fixtures.Cp1252 as Cp1252
import Fixtures.OpenSans as OpenSans
import Fuzz as F
import Helpers.Unit as Unit
import Test as T exposing (Test)


codePage : Test
codePage =
    T.describe "Text.CodePage Module"
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
    T.describe "Text.Font Module"
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


text : Test
text =
    T.describe "Text Module"
        [ T.describe "shrink"
            [ T.fuzz
                (F.tuple
                    ( F.list <| Unit.fuzzer Cp1252.codePage OpenSans.font 16
                    , F.tuple ( F.floatRange 50 9999, F.floatRange 100 9999 )
                    )
                )
                "always keep text paragraphs height under max value"
              <|
                -- NOTE that width can overflow if a word is unbreakable
                \( paragraph, ( width, height ) ) ->
                    paragraph
                        |> Text.shrink
                            { size = { width = width, height = height }
                            , scaleFactor = 0.05
                            , maxSteps = 64
                            }
                        |> Unit.boundingSize
                        |> .height
                        |> E.lessThan height
            ]
        , T.describe "wrap"
            [ T.test "returns a well known string wrapped" <|
                \() ->
                    "To The Moon"
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> Text.wrap 50
                        |> Unit.toParagraph
                        |> E.equal [ [ "To", " ", "The" ], [ "Moon" ] ]
            , T.test "wrap a well known unbreakable paragraph (regression)" <|
                \() ->
                    "ItsOverNineThousand WowToTheMoon Yeah"
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> Text.wrap 50
                        |> Unit.toParagraph
                        |> E.equal [ [ "ItsOverNineThousand" ], [ "WowToTheMoon" ], [ "Yeah" ] ]
            , T.fuzz
                (F.tuple
                    ( F.list <| Unit.fuzzer Cp1252.codePage OpenSans.font 16
                    , F.floatRange 0 9999
                    )
                )
                "wraps any text"
              <|
                \( paragraph, width ) ->
                    paragraph
                        |> Text.wrap width
                        |> Unit.lineStats
                        |> List.map
                            (\{ size, count } ->
                                always <| E.false "width larger than expected" (size.width > width && count > 1)
                            )
                        |> (\expectations -> E.all expectations ())
            ]
        , T.describe "trim"
            [ T.test "trims a well known string" <|
                \() ->
                    " To The Moon   \n   wow "
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> Text.trim
                        |> Unit.toParagraph
                        |> E.equal [ [ "To", " ", "The", " ", "Moon" ], [ "wow" ] ]
            ]
        ]


unit : Test
unit =
    T.describe "Text.Unit Module"
        [ T.describe "fromString"
            [ T.test "returns an empty list when an empty string is provided" <|
                \() ->
                    ""
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> E.equal []
            , T.test "manages a well known string" <|
                \() ->
                    " To the  Moon   "
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> List.map (Unit.text >> Maybe.withDefault "")
                        |> E.equal [ " ", "To", " ", "the", "  ", "Moon", "   " ]
            , T.fuzz (F.intRange 1 9999) "keeps spaces at the beginning of the string" <|
                \spacesLength ->
                    (String.repeat spacesLength " " ++ "foobar")
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> E.all
                            [ List.length >> E.equal 2
                            , List.head
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
                            [ List.length >> E.equal 2
                            , List.reverse
                                >> List.head
                                >> Maybe.andThen Unit.text
                                >> Maybe.withDefault ""
                                >> String.length
                                >> E.equal spacesLength
                            ]
            ]
        , T.describe "isWhitespace"
            [ T.fuzz (F.intRange 1 999) "detects Word units with whitespace characters" <|
                \count ->
                    " "
                        |> String.repeat count
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> List.head
                        |> Maybe.map Unit.isWhitespace
                        |> E.equal (Just True)
            , T.test "detects Inline units with scale equal to zero" <|
                \() ->
                    ()
                        |> Unit.embed 0 { width = 1, height = 1 } Geometry.zeroOffset
                        |> Unit.isWhitespace
                        |> E.equal True
            , T.test "detects Inline units with width equal to zero" <|
                \() ->
                    ()
                        |> Unit.embed 1 { width = 0, height = 1 } Geometry.zeroOffset
                        |> Unit.isWhitespace
                        |> E.equal True
            , T.test "detects Inline units with height equal to zero" <|
                \() ->
                    ()
                        |> Unit.embed 1 { width = 1, height = 0 } Geometry.zeroOffset
                        |> Unit.isWhitespace
                        |> E.equal True
            ]
        , T.describe "joinWords"
            [ T.test "joins adjacent word of a well-known paragraph" <|
                \() ->
                    "To The Moon\nWow"
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> Unit.joinWords
                        |> Unit.toParagraph
                        |> E.equal [ [ "To The Moon" ], [ "Wow" ] ]
            , T.test "avoids joining multiple spaces together" <|
                \() ->
                    "The  Moon"
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> Unit.joinWords
                        |> Unit.toParagraph
                        |> E.equal [ [ "The", "  ", "Moon" ] ]
            ]
        , T.describe "lines"
            [ T.test "split lines of a well-known paragraph" <|
                \() ->
                    "To The Moon\nWow"
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> Unit.lines
                        |> List.map Unit.toParagraph
                        |> E.equal [ [ [ "To", " ", "The", " ", "Moon" ] ], [ [ "Wow" ] ] ]
            ]
        , T.describe "size"
            [ T.test "return the proper size of a well-known Word Unit" <|
                \() ->
                    "elm-composer"
                        |> Unit.fromString Cp1252.codePage OpenSans.font 16
                        |> List.head
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
        ]
