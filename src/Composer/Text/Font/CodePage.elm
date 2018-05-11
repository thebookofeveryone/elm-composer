module Composer.Text.Font.CodePage
    exposing
        ( CodePage
        , codepoint
        , empty
        , fromString
        , index
        )

{-| A [CodePage](https://en.wikipedia.org/wiki/Character_encoding) is an
association between a set of characters to encoding system.

Usually Fonts have glyph information in tables, for example, the table of
glyph widths. To obtain to the width of a glyph we have to use a CodePage to
get the index that contains the width in a font table.

@docs CodePage


# Quering

@docs codepoint, index


# Creating encodings

@docs empty, fromString

-}

import Char
import Dict exposing (Dict)
import Hex
import Parser as P exposing (Parser, (|.), (|=))


{-| An opaque type (or a private type) that contains the mapping between a
character set and code points (values of an encoding).
-}
type CodePage
    = CodePage
        { codepoints : Dict Char Int
        , indexes : Dict Int Char
        }


{-| Given an encoding value (or code point) returns the character associated to
that value, if exists.
-}
index : Int -> CodePage -> Maybe Char
index index (CodePage { indexes }) =
    Dict.get index indexes


{-| Given a character, return the encoding value of that character, if exists.
-}
codepoint : Char -> CodePage -> Maybe Int
codepoint char (CodePage { codepoints }) =
    Dict.get char codepoints


{-| Parses a string containing a code page. The format used to represent
CodePages is a textual representation used by
[gofpdf](https://github.com/jung-kurt/gofpdf). Here is a
[CodePage Example](https://github.com/jung-kurt/gofpdf/blob/master/font/cp1250.map)
file.
-}
fromString : String -> Result String CodePage
fromString string =
    string
        |> P.run entryList
        |> Result.map (List.foldl addEntry empty)
        |> Result.mapError (always "parsing error")


{-| An empty CodePage. Useful for testing.
-}
empty : CodePage
empty =
    CodePage
        { codepoints = Dict.empty
        , indexes = Dict.empty
        }


addEntry : Entry -> CodePage -> CodePage
addEntry entry (CodePage { codepoints, indexes }) =
    let
        char =
            Char.fromCode entry.codepoint
    in
        CodePage
            { codepoints = Dict.insert char entry.index codepoints
            , indexes = Dict.insert entry.index char indexes
            }



-- Entry Parsing --


type alias Entry =
    { index : Int
    , codepoint : Int
    , name : String
    }


entryList : Parser (List Entry)
entryList =
    P.repeat P.zeroOrMore entry


entry : Parser Entry
entry =
    P.succeed Entry
        |. whitespace
        |= indexValue
        |. whitespace
        |= unicodeValue
        |. whitespace
        |= nameValue
        |. whitespace


indexValue : Parser Int
indexValue =
    P.succeed identity
        |. P.symbol "!"
        |= hex


nameValue : Parser String
nameValue =
    P.keep P.oneOrMore <|
        \char ->
            Char.isLower char
                || Char.isUpper char
                || Char.isDigit char
                || (char == '_')
                || (char == ':')
                || (char == '.')


unicodeValue : Parser Int
unicodeValue =
    P.succeed identity
        |. P.oneOf [ P.symbol "U+", P.symbol "u+" ]
        |= hex



-- Generic Parsing Units --


whitespace : Parser ()
whitespace =
    P.ignore P.zeroOrMore (\chr -> List.member chr whitespaceCharSet)


hex : Parser Int
hex =
    P.andThen
        (\str ->
            case Hex.fromString (String.toLower str) of
                Ok num ->
                    P.succeed num

                Err err ->
                    P.fail err
        )
        (P.keep P.oneOrMore <| \chr -> List.member chr hexCharSet)



-- Character Sets --


whitespaceCharSet : List Char
whitespaceCharSet =
    String.toList " \n\x0D\t"


hexCharSet : List Char
hexCharSet =
    String.toList "0123456789ABCDEFabcdef"


extraNameCharSet : List Char
extraNameCharSet =
    String.toList " \n\x0D\t"
