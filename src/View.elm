module View exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (on)
import Json.Decode as JD


onInputAsNumber : (Int -> msg) -> Html.Attribute msg
onInputAsNumber msg =
    on "input" <|
        (JD.at [ "target", "valueAsNumber" ] JD.int |> JD.map msg)
