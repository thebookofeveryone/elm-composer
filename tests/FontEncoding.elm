module FontEncoding exposing (suite)

import Composer.Text.Font.Encoding as Encoding exposing (Encoding)
import Expect as E
import Helpers.Result as Result
import Test as T exposing (Test)


suite : Test
suite =
    T.describe "Composer.Text.Font.Encoding Decoder"
        [ T.test "decodes a valid file" <|
            \() ->
                """
                  !1F U+001F .notdef
                  !3F U+003F question
                  !40 U+0040 at
                  !41 U+0041 A
                  !42 U+0042 B
                  !43 U+0043 C
                  !61 U+0061 a
                  !62 U+0062 b
                  !63 U+0063 c
                """
                    |> Encoding.fromString
                    |> E.all
                        [ mapWithEmpty >> Encoding.index '?' >> E.equal (Just 0x3F)
                        , mapWithEmpty >> Encoding.index 'a' >> E.equal (Just 0x61)
                        , mapWithEmpty >> Encoding.codepoint 0x61 >> E.equal (Just 'a')
                        ]
        ]


mapWithEmpty : Result err Encoding -> Encoding
mapWithEmpty =
    Result.mapWithDefault Encoding.empty identity
