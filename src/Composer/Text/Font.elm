module Composer.Text.Font
    exposing
        ( Description
        , Font
        , Type(..)
        , decoder
        , empty
        , glyphWidth
        , kerning
        , stringWidth
        )

{-| A module for reading [font](https://en.wikipedia.org/wiki/Computer_font)
descriptions.

Composer only need to known about some font properties in order to layout text.

This module cannot open standard font files, like TrueType (ttf), OpenType
(otf), etc. Instead a JSON definition should be provided. The format of this
definition is the same used by the
[gofpdf](https://godoc.org/github.com/jung-kurt/gofpdf#hdr-Nonstandard_Fonts)
project. Use the `makefont` utility provided by *gofpdf* to create this
definitions.

@docs Description, Font, Type


# Loading Fonts

@docs decoder, empty


# Querying Fonts

@docs glyphWidth, kerning, stringWidth

-}

import Array exposing (Array)
import Char
import Composer.Geometry exposing (BoundingBox)
import Composer.Text.Font.CodePage as CodePage exposing (CodePage)
import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)


{-| A description containing information common to all font glyphs.

  - `ascent`: the distance above the baseline for singled spaced text.
  - `boundingBox`: a bounding box that can contains any glyph.
  - `capHeight`: the height of a capital letter above the baseline
  - `descent`: the distance below the baseline for singled spaced text.
  - `italicAngle`: the angle of the italic version of the font.
  - `missingWidth`: the width we should assume for a glyph not found in the
    font.

-}
type alias Description =
    -- ignored: Flags, StemV
    { ascent : Float
    , boundingBox : BoundingBox
    , capHeight : Float
    , descent : Float
    , italicAngle : Float
    , missingWidth : Float
    }


{-| A font record.

  - `description`: common information for all glyphs.
  - `kernings`: distance between character pairs.
  - `name`: the font name, in English.
  - `type_`: the underlying font type.
  - `widths`: the width of each glyph, use a [CodePage] to resolve a character
    index.

-}
type alias Font =
    -- ignored: Up (Underline Position), Ut (Underline Thickness),
    -- Diff (Encoding Differences), File, Size1, Size2, OriginalSize, N and I.
    { description : Description
    , kernings : Dict ( Int, Int ) Float
    , name : String
    , type_ : Type
    , widths : Array Float
    }


{-| The format type of type.
-}
type Type
    = TrueType
    | OpenType


{-| An empty Font. Useful for testing.
-}
empty : Font
empty =
    { description =
        { ascent = 0
        , boundingBox =
            { xMax = 0
            , xMin = 0
            , yMax = 0
            , yMin = 0
            }
        , capHeight = 0
        , descent = 0
        , italicAngle = 0
        , missingWidth = 0
        }
    , kernings = Dict.empty
    , name = ""
    , type_ = TrueType
    , widths = Array.empty
    }


{-| The JSON decoder for a Font. The
[gofpdf](https://godoc.org/github.com/jung-kurt/gofpdf#hdr-Nonstandard_Fonts)
format is used.
-}
decoder : Decoder Font
decoder =
    JD.map5 Font
        (JD.field "Desc" descriptionDecoder)
        (JD.field "Ck" kerningsDecoder)
        (JD.field "Name" JD.string)
        (JD.field "Tp" typeDecoder)
        (JD.field "Cw" <| JD.map Array.fromList <| JD.list JD.float)


{-| Returns the glyph with give an character. A CodePage is also needed to
resolve the character codepoint.
-}
glyphWidth : CodePage -> Font -> Char -> Float
glyphWidth codePage font char =
    case CodePage.codepoint char codePage of
        Just codepoint ->
            font.widths
                |> Array.get codepoint
                |> Maybe.withDefault font.description.missingWidth

        Nothing ->
            font.description.missingWidth


{-| Returns the [kerning](https://en.wikipedia.org/wiki/Kerning) given a pair of
chars. A CodePage is also needed to resolve the character codepoint.
-}
kerning : CodePage -> Font -> Char -> Char -> Float
kerning codePage font lhsChar rhsChar =
    case ( CodePage.codepoint lhsChar codePage, CodePage.codepoint rhsChar codePage ) of
        ( Just lhsIndex, Just rhsIndex ) ->
            font.kernings
                |> Dict.get ( lhsIndex, rhsIndex )
                |> Maybe.withDefault 0

        _ ->
            0


{-| Returns the with of a given string. This function is similar to glyphWidth
but also takes into account the glyphs kerning.
-}
stringWidth : CodePage -> Font -> String -> Float
stringWidth codePage font string =
    let
        stringList =
            String.toList string

        glyphListWidth =
            stringList
                |> List.map (glyphWidth codePage font)
                |> List.sum

        kerningListWidth =
            stringList
                |> List.tail
                |> Maybe.withDefault []
                |> List.map2 (,) stringList
                |> List.map (\( lhs, rhs ) -> kerning codePage font lhs rhs)
                |> List.sum
    in
        glyphListWidth + kerningListWidth


boundingBoxDecoder : Decoder BoundingBox
boundingBoxDecoder =
    JD.map4 BoundingBox
        (JD.field "Xmax" JD.float)
        (JD.field "Xmin" JD.float)
        (JD.field "Ymax" JD.float)
        (JD.field "Ymin" JD.float)


descriptionDecoder : Decoder Description
descriptionDecoder =
    JD.map6 Description
        (JD.field "Ascent" JD.float)
        (JD.field "FontBBox" boundingBoxDecoder)
        (JD.field "CapHeight" JD.float)
        (JD.field "Descent" JD.float)
        (JD.field "ItalicAngle" JD.float)
        (JD.field "MissingWidth" JD.float)


kerningsDecoder : Decoder (Dict ( Int, Int ) Float)
kerningsDecoder =
    let
        insertKerningLine : ( String, List Float ) -> Dict ( Int, Int ) Float -> Dict ( Int, Int ) Float
        insertKerningLine ( indexString, kernList ) dict =
            case String.toInt indexString of
                Ok lhs ->
                    kernList
                        |> zip
                        |> List.foldl
                            (\( rhs, kerning ) acc ->
                                Dict.insert ( lhs, rhs ) kerning acc
                            )
                            dict

                Err _ ->
                    dict

        zip : List Float -> List ( Int, Float )
        zip list =
            list
                |> List.tail
                |> Maybe.withDefault []
                |> List.map2 (,) list
                |> List.map (Tuple.mapFirst round)
    in
        JD.dict (JD.list JD.float)
            |> JD.andThen
                (\dict ->
                    dict
                        |> Dict.toList
                        |> List.foldl insertKerningLine Dict.empty
                        |> JD.succeed
                )


typeDecoder : Decoder Type
typeDecoder =
    JD.string
        |> JD.andThen
            (\value ->
                case value of
                    "TrueType" ->
                        JD.succeed TrueType

                    "OpenType" ->
                        JD.succeed OpenType

                    _ ->
                        JD.fail ("unknown \"" ++ value ++ "font type")
            )
