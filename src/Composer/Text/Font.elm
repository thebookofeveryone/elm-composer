module Composer.Text.Font
    exposing
        ( BoundingBox
        , Description
        , Font
        , Type(..)
        , decoder
        )

{-| TODO

@docs BoundingBox, Description, Font, Type, decoder

-}

import Json.Decode as JD exposing (Decoder)


{-| TODO -
-}
type alias BoundingBox =
    { xMax : Float
    , xMin : Float
    , yMax : Float
    , yMin : Float
    }


{-| TODO -
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


{-| TODO -
-}
type alias Font =
    -- ignored: Up (Underline Position), Ut (Underline Thickness),
    -- Diff (Encoding Differences), File, Size1, Size2, OriginalSize, N and I.
    { description : Description
    , name : String
    , type_ : Type
    }


{-| TODO -
-}
type Type
    = TrueType
    | OpenType


{-| TODO -
-}
decoder : Decoder Font
decoder =
    JD.map3 Font
        (JD.field "Desc" descriptionDecoder)
        (JD.field "Name" JD.string)
        (JD.field "Tp" typeDecoder)


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
