module NewPartsForm exposing (..)

import Dict
import Form.Decoder as Decoder
import Types


type PartsType
    = Distance
    | Both


type alias InputForm =
    { name : String
    , type_ : PartsType
    , distance : String
    , day : String
    }


type alias Decoded =
    { name : String
    , type_ : PartsType
    , distance : Int
    , day : Int
    }


name_ : Decoder.Decoder InputForm String String
name_ =
    Decoder.identity
        |> Decoder.assert (Decoder.minLength "name empty" 1)
        |> Decoder.lift .name


type__ : Decoder.Decoder InputForm String PartsType
type__ =
    Decoder.identity |> Decoder.lift .type_


distance_ : Decoder.Decoder InputForm String Int
distance_ =
    Decoder.int "must be string"
        |> Decoder.assert (Decoder.minBound "greater than 1" 1)
        |> Decoder.lift .distance


day_ : Decoder.Decoder InputForm String Int
day_ =
    Decoder.int "must be string"
        |> Decoder.assert (Decoder.minBound "greater than 1" 1)
        |> Decoder.lift .day


form_ : PartsType -> Decoder.Decoder InputForm String Types.Parts
form_ t =
    case t of
        Both ->
            Decoder.map4 Types.PartsBothArg name_ distance_ day_ (Decoder.always Dict.empty)
                |> Decoder.map Types.PartsBoth

        Distance ->
            Decoder.map3 Types.PartsDistanceArg name_ distance_ (Decoder.always Dict.empty)
                |> Decoder.map Types.PartsDistanceOnly


formDecoder : Decoder.Decoder InputForm String Types.Parts
formDecoder =
    type__ |> Decoder.andThen form_
