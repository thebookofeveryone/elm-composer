module Helpers.String exposing (wordsAndSpaces)

import Helpers.Char as Char


{-| Given a string, returns a list of string with all the whitespaces grouped.

    wordsAndSpaces " to the  moon   "
    --> [" ", "to", "the", "  ", "moon", "   "]

Note that using `Regex.split` remove the space characters, so is not an
alternative.

-}
wordsAndSpaces : String -> List String
wordsAndSpaces string =
    string
        |> String.foldr
            (\char state ->
                if Char.isWhitespace char then
                    case ( state.maybeSpace, state.maybeWord ) of
                        ( Just spaces, Nothing ) ->
                            { list = state.list
                            , maybeSpace = Just (String.cons char spaces)
                            , maybeWord = Nothing
                            }

                        ( Nothing, Just word ) ->
                            { list = word :: state.list
                            , maybeSpace = Just (String.fromChar char)
                            , maybeWord = Nothing
                            }

                        ( _, _ ) ->
                            { list = state.list
                            , maybeSpace = Just (String.fromChar char)
                            , maybeWord = Nothing
                            }
                else
                    case ( state.maybeSpace, state.maybeWord ) of
                        ( Nothing, Just word ) ->
                            { list = state.list
                            , maybeSpace = Nothing
                            , maybeWord = Just (String.cons char word)
                            }

                        ( Just spaces, Nothing ) ->
                            { list = spaces :: state.list
                            , maybeSpace = Nothing
                            , maybeWord = Just (String.fromChar char)
                            }

                        ( _, _ ) ->
                            { list = state.list
                            , maybeSpace = Nothing
                            , maybeWord = Just (String.fromChar char)
                            }
            )
            { list = [], maybeSpace = Nothing, maybeWord = Nothing }
        |> \{ list, maybeSpace, maybeWord } ->
            case ( maybeSpace, maybeWord ) of
                ( Just spaces, _ ) ->
                    spaces :: list

                ( _, Just word ) ->
                    word :: list

                ( Nothing, Nothing ) ->
                    list
