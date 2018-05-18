module Composer.Text.Unit
    exposing
        ( Unit(Word, Inline)
        , fromString
        , size
        , text
        , toString
        )

{-| A text unit, the basic element for the layout algorithm.

@docs Unit


# Creating Units

@docs fromString


# Querying Units

@docs size, text, toString

-}

import Composer.Geometry exposing (Offset, Size)
import Composer.Text.Font as Font exposing (Font)
import Composer.Text.Font.CodePage as CodePage exposing (CodePage)
import Helpers.Char as Char
import Helpers.String as String


{-| A basic unit for the text layout algorithm. Usually this a non divisible
word, but it can also be in-line objects like, for example, images.
-}
type Unit inline
    = Word
        { codePage : CodePage
        , font : Font
        , fontSize : Float
        , text : String
        }
    | Inline
        { content : inline
        , scale : Float
        , size : Size
        , offset : Offset
        }


{-| Returns true if the unit is a whitespace. One of the following conditions
must be true if a Unit is a whitespace:

  - All characters the text of a Word Unit are white spaces.
  - The fontSize of a Word Unit is less than zero.
  - A Inline Unit with scale equal to zero.
  - A Inline Unit with height or width equal to zero.

-}
isWhitespace : Unit inline -> Bool
isWhitespace unit =
    case unit of
        Word word ->
            (word.fontSize <= 0)
                || (String.any (not << Char.isWhitespace) word.text)

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
                Font.stringWidth word.codePage word.font word.text * word.fontSize
            , height =
                (word.font.description.boundingBox.yMax
                    - word.font.description.boundingBox.yMin
                )
                    * word.fontSize
            }

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

        Inline _ ->
            Nothing


{-| Converts a string into a bunch of units. A Font, CodePage and FontSize are
also needed.
-}
fromString : CodePage -> Font -> Float -> String -> List (List (Unit any))
fromString codePage font fontSize text =
    let
        toUnit word =
            Word { text = word, fontSize = fontSize, font = font, codePage = codePage }
    in
        text
            |> String.lines
            |> List.map (String.wordsAndSpaces >> List.map toUnit)


{-| Returns a textual representation of a unit. Useful for testing.
-}
toString : Unit any -> String
toString unit =
    case unit of
        Word { font, fontSize, text } ->
            "<word \"" ++ text ++ "\" (" ++ font.name ++ ", " ++ Basics.toString fontSize ++ ")>"

        Inline { size, scale } ->
            "<inline (" ++ Basics.toString size.width ++ "x" ++ Basics.toString size.height ++ ", " ++ Basics.toString scale ++ ")>"
