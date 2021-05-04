module PartsLogForm exposing (..)

import Form.Decoder as Decoder
import Time
import Types


type alias PartsLogForm =
    { day : Maybe Int, distance : Maybe String }


partsLogFormDefault : PartsLogForm
partsLogFormDefault =
    PartsLogForm Nothing Nothing


day_ : Decoder.Decoder Int String Time.Posix
day_ =
    Decoder.identity |> Decoder.map (\d -> d |> Time.millisToPosix)


distance_ : Decoder.Decoder String String Types.Distance
distance_ =
    Decoder.int "must be a number"


liftMaybe : err -> Decoder.Decoder source err a -> Decoder.Decoder (Maybe source) err a
liftMaybe err dec =
    Decoder.custom <|
        \source ->
            case source of
                Nothing ->
                    Err [ err ]

                Just s ->
                    Decoder.run dec s


partsLogFormDecoder : Decoder.Decoder PartsLogForm String Types.PartsBothHistoryElem
partsLogFormDecoder =
    Decoder.map2
        Types.PartsBothHistoryElem
        (distance_ |> liftMaybe "please select" |> Decoder.lift .distance)
        (day_ |> liftMaybe "please select" |> Decoder.lift .day)
