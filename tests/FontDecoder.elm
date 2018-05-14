module FontDecoder exposing (suite)

import Array
import Composer.Text.Font as Font
import Expect as E
import Helpers.Result exposing (mapWithDefault)
import Json.Decode as JD
import Test as T exposing (Test)


suite : Test
suite =
    T.describe "Composer.Text.Font Json Decoder"
        [ T.test "decodes a valid json" <|
            \() ->
                """
                  {
                     "Tp":"TrueType",
                     "Name":"Pattaya-Regular",
                     "Desc":{
                        "Ascent":800,
                        "Descent":-200,
                        "CapHeight":748,
                        "Flags":32,
                        "FontBBox":{
                           "Xmin":-209,
                           "Ymin":-250,
                           "Xmax":1032,
                           "Ymax":1000
                        },
                        "ItalicAngle":0.5,
                        "StemV":70,
                        "MissingWidth":365
                     },
                     "Up":-274,
                     "Ut":50,
                     "Cw":[ 365, 365, 365, 360, 365, 365, 365, 365, 365 ],
                     "Enc":"cp1252",
                     "Diff":"",
                     "File":"",
                     "Size1":0,
                     "Size2":0,
                     "OriginalSize":0,
                     "I":0,
                     "N":0,
                     "DiffN":0
                  }
                """
                    |> JD.decodeString Font.decoder
                    |> E.all
                        [ mapWithDefault 0 (.description >> .ascent) >> E.equal 800
                        , mapWithDefault 0 (.description >> .boundingBox >> .xMax) >> E.equal 1032
                        , mapWithDefault 0 (.description >> .boundingBox >> .xMin) >> E.equal -209
                        , mapWithDefault 0 (.description >> .boundingBox >> .yMax) >> E.equal 1000
                        , mapWithDefault 0 (.description >> .boundingBox >> .yMin) >> E.equal -250
                        , mapWithDefault 0 (.description >> .capHeight) >> E.equal 748
                        , mapWithDefault 0 (.description >> .descent) >> E.equal -200
                        , mapWithDefault 0 (.description >> .italicAngle) >> E.equal 0.5
                        , mapWithDefault 0 (.description >> .missingWidth) >> E.equal 365
                        , mapWithDefault 0 (.widths >> Array.get 3 >> Maybe.withDefault 0) >> E.equal 360
                        , mapWithDefault "" .name >> E.equal "Pattaya-Regular"
                        , mapWithDefault Font.OpenType .type_ >> E.equal Font.TrueType
                        ]
        ]
