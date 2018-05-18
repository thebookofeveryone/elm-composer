module Composer.Text.Unit
    exposing
        ( Unit(Inline, LineBreak, Word)
        , embed
        , fromString
        , isWhitespace
        , size
        , text
        , toString
        )

{-| A text unit, the basic element for the layout algorithm.

@docs Unit


# Creating Units

@docs fromString, embed


# Querying Units

@docs isWhitespace, size, text, toString

-}

import Composer.Geometry exposing (Offset, Size)
import Composer.Text.Font as Font exposing (Font)
import Composer.Text.Font.CodePage as CodePage exposing (CodePage)
import Helpers.Char as Char
import Helpers.String as String


{-| A basic unit for the text layout algorithm. Usually this a non divisible
word, but it can also be in-line objects like, for example, images or a line
break.
-}
type Unit inline
    = Inline
        { content : inline
        , scale : Float
        , size : Size
        , offset : Offset
        }
    | LineBreak
    | Word
        { codePage : CodePage
        , font : Font
        , fontSize : Float
        , text : String
        }


{-| Embed any content into a unit, creating an Inline unit. The size, scale and
an offset is also needed.
-}
embed : Float -> Size -> Offset -> content -> Unit content
embed scale size offset content =
    Inline
        { content = content
        , scale = scale
        , size = size
        , offset = offset
        }


{-| Returns true if the unit is a whitespace. One of the following conditions
must be true if a Unit is a whitespace:

  - A word unit with no characters (There is no way to create these kind of
    units).
  - All characters the text of a Word Unit are white spaces.
  - The fontSize of a Word Unit is less than zero.
  - A Inline Unit with scale equal to zero.
  - A Inline Unit with height or width equal to zero.
  - Is a line break.

-}
isWhitespace : Unit inline -> Bool
isWhitespace unit =
    case unit of
        Word word ->
            (String.length word.text <= 0)
                || (word.fontSize <= 0)
                || not (String.any (not << Char.isWhitespace) word.text)

        LineBreak ->
            True

        Inline inline ->
            (inline.scale == 0)
                || (inline.size.width == 0)
                || (inline.size.height == 0)


{-| Returns the size of a unit.
-}
size : Unit inline -> Size
size unit =
    case unit of
        Word word ->
            { width =
                Font.stringWidth word.codePage word.font word.text / 1000 * word.fontSize
            , height =
                (word.font.description.boundingBox.yMax - word.font.description.boundingBox.yMin)
                    / 1000
                    * word.fontSize
            }

        LineBreak ->
            { width = 0, height = 0 }

        Inline inline ->
            { width = inline.size.width * inline.scale
            , height = inline.size.height * inline.scale
            }


{-| Returns the text of an Unit, if any.
-}
text : Unit inline -> Maybe String
text unit =
    case unit of
        Word { text } ->
            Just text

        LineBreak ->
            Nothing

        Inline _ ->
            Nothing


{-| Converts a string into a bunch of units. A Font, CodePage and FontSize are
also needed.
-}
fromString : CodePage -> Font -> Float -> String -> List (Unit any)
fromString codePage font fontSize text =
    let
        toUnit word =
            Word { text = word, fontSize = fontSize, font = font, codePage = codePage }
    in
        text
            |> String.lines
            |> List.map (String.wordsAndSpaces >> List.map toUnit)
            |> List.intersperse (List.singleton LineBreak)
            |> List.concat


{-| Returns a textual representation of a unit. Useful for testing.
-}
toString : Unit any -> String
toString unit =
    let
        unitSize =
            size unit

        unitSizeString =
            "(" ++ Basics.toString unitSize.width ++ "x" ++ Basics.toString unitSize.height ++ ")"
    in
        case unit of
            Inline _ ->
                "<inline '" ++ unitSizeString ++ ">"

            LineBreak ->
                "<\\n>"

            Word { text } ->
                "<word '" ++ text ++ "' " ++ unitSizeString ++ ">"
