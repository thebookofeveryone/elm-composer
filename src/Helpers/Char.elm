module Helpers.Char exposing (isWhitespace)

import Char


{-| Detect white space characters, including tab, form feed, line feed and other
Unicode spaces. Equivalent to the [whitespace][ws] (`\s`) regex class.

[ws]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp#character-classes

    isWhitespace ' ' == True
    isWhitespace '\n' == True
    isWhitespace '\t' == True
    isWhitespace '0x' == True
    isWhitespace '-' == False

-}
isWhitespace : Char -> Bool
isWhitespace char =
    let
        code =
            Char.toCode char
    in
        -- ' '
        (code == 32)
            -- '\n'
            || (code == 10)
            -- '\t'
            || (code == 9)
            -- '\f'
            || (code == 7)
            -- '\v'
            || (code == 11)
            -- '\r'
            || (code == 13)
            -- Non ASCII Spaces
            || (code == 0xA0)
            || (code == 0x1680)
            || (code >= 0x2000 && code <= 0x200A)
            || (code == 0x2028)
            || (code == 0x2029)
            || (code == 0x202F)
            || (code == 0x205F)
            || (code == 0x3000)
            || (code == 0xFEFF)
