module Util exposing (..)


withSuffix : String -> Int -> String
withSuffix suffix num =
    String.fromInt num |> (\s -> s ++ suffix)
