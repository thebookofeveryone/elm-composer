module Composer.Text.Font
    exposing
        ( Description
        , Font
        , Type(..)
        , decoder
        , empty
        , glyphWidth
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

@docs glyphWidth

-}

import Array exposing (Array)
import Composer.Geometry exposing (BoundingBox)
import Json.Decode as JD exposing (Decoder)
import Composer.Text.Font.CodePage as CodePage exposing (CodePage)


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
  - `name`: the font name, in English.
  - `type_`: the underlying font type.
  - `widths`: the width of each glyph, use a [CodePage] to resolve a character
    index.

-}
type alias Font =
    -- ignored: Up (Underline Position), Ut (Underline Thickness),
    -- Diff (Encoding Differences), File, Size1, Size2, OriginalSize, N and I.
    { description : Description
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
    JD.map4 Font
        (JD.field "Desc" descriptionDecoder)
        (JD.field "Name" JD.string)
        (JD.field "Tp" typeDecoder)
        (JD.field "Cw" <| JD.map Array.fromList <| JD.list JD.float)


{-| Returns the glyph with give an character. A CodePage is also needed to
resolve the character codepoint.
-}
glyphWidth : Char -> CodePage -> Font -> Float
glyphWidth char codePage font =
    case CodePage.codepoint char codePage of
        Nothing ->
            font.description.missingWidth

        Just codepoint ->
            font.widths
                |> Array.get codepoint
                |> Maybe.withDefault font.description.missingWidth


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
