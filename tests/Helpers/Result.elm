module Helpers.Result exposing (mapWithDefault)


mapWithDefault : b -> (a -> b) -> Result err a -> b
mapWithDefault default fn result =
    result |> Result.map fn |> Result.withDefault default
