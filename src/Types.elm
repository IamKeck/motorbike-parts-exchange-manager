module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Encode as JE
import Random exposing (Generator)
import Time
import UUID
import Util


type alias Distance =
    Int



-- millisecond


type alias Day =
    Int


type alias Bike =
    { name : String, parts : Parts }


type alias PartsBothArg =
    { name : String, distance : Distance, day : Day, history : PartsBothHistory }


type alias PartsDistanceArg =
    { name : String, distance : Distance, history : PartsBothHistory }


type Parts
    = PartsBoth PartsBothArg
    | PartsDistanceOnly PartsDistanceArg


type alias PartsKey =
    String


type alias PartsDict =
    Dict PartsKey Parts


type alias Date =
    Time.Posix


type alias HistoryKey =
    String


type alias PartsBothHistoryElem =
    { distance : Distance, day : Date }


type alias PartsBothHistory =
    Dict.Dict HistoryKey PartsBothHistoryElem


type Status
    = ReplaceRequired
    | OkDistance Distance
    | OkBoth { day : Date, distance : Distance }


partsStatus : Time.Posix -> Parts -> Status
partsStatus now parts =
    case parts of
        PartsDistanceOnly record ->
            case getLatestHistory record.history of
                Nothing ->
                    OkDistance record.distance

                Just latestHistory ->
                    OkDistance <| latestHistory.distance + record.distance

        PartsBoth record ->
            case getLatestHistory record.history of
                Nothing ->
                    OkBoth <| { day = addDay record.day now, distance = record.distance }

                Just latestHistory ->
                    let
                        diffDay =
                            Debug.log "diffD" <| Time.posixToMillis now - Time.posixToMillis latestHistory.day
                    in
                    if diffDay >= record.day then
                        ReplaceRequired

                    else
                        OkBoth
                            { day = addDay record.day latestHistory.day
                            , distance = record.distance + latestHistory.distance
                            }


generatePartsKey : Generator PartsKey
generatePartsKey =
    UUID.generator |> Random.map UUID.toString


generateHistoryKey : Generator HistoryKey
generateHistoryKey =
    UUID.generator |> Random.map UUID.toString


addNewPartsToDict : PartsKey -> Parts -> PartsDict -> PartsDict
addNewPartsToDict =
    Dict.insert


addNewLogToDict : PartsKey -> PartsDict -> HistoryKey -> PartsBothHistoryElem -> PartsDict
addNewLogToDict key dict hisKey history =
    let
        f : Parts -> Parts
        f parts =
            case parts of
                PartsBoth arg ->
                    PartsBoth <| { arg | history = addNewHistory hisKey history arg.history }

                PartsDistanceOnly arg ->
                    PartsDistanceOnly <| { arg | history = addNewHistory hisKey history arg.history }
    in
    Dict.update key (Maybe.map f) dict


showMonth : Time.Month -> String
showMonth m =
    case m of
        Time.Jan ->
            "1月"

        Time.Feb ->
            "2月"

        Time.Mar ->
            "3月"

        Time.Apr ->
            "4月"

        Time.May ->
            "5月"

        Time.Jun ->
            "6月"

        Time.Jul ->
            "7月"

        Time.Aug ->
            "8月"

        Time.Sep ->
            "9月"

        Time.Oct ->
            "10月"

        Time.Nov ->
            "11月"

        Time.Dec ->
            "12月"


showDate : Date -> String
showDate d =
    let
        year =
            Time.toYear Time.utc d |> Util.withSuffix "年"

        month =
            Time.toMonth Time.utc d |> showMonth

        day =
            Time.toDay Time.utc d |> Util.withSuffix "日"
    in
    year ++ month ++ day



-- convert partsDict into json


saveParts : PartsDict -> String
saveParts parts =
    let
        historyElemEncoder : PartsBothHistoryElem -> JE.Value
        historyElemEncoder hist =
            JE.object
                [ ( "distance", JE.int hist.distance )
                , ( "day", JE.int <| Time.posixToMillis hist.day )
                ]

        historyEncoder : PartsBothHistory -> JE.Value
        historyEncoder hist =
            JE.dict identity historyElemEncoder hist

        partsEncoder : Parts -> JE.Value
        partsEncoder p =
            case p of
                PartsBoth arg ->
                    JE.object
                        [ ( "type", JE.string "both" )
                        , ( "name", JE.string arg.name )
                        , ( "distance", JE.int arg.distance )
                        , ( "day", JE.int arg.day )
                        , ( "history", historyEncoder arg.history )
                        ]

                PartsDistanceOnly arg ->
                    JE.object
                        [ ( "type", JE.string "distance" )
                        , ( "name", JE.string arg.name )
                        , ( "distance", JE.int arg.distance )
                        , ( "history", historyEncoder arg.history )
                        ]

        partsDictEncoder : PartsDict -> JE.Value
        partsDictEncoder =
            JE.dict identity partsEncoder
    in
    JE.encode 0 <| partsDictEncoder parts


restoreParts : String -> Result JD.Error PartsDict
restoreParts json =
    let
        historyElemDecoder : JD.Decoder PartsBothHistoryElem
        historyElemDecoder =
            JD.map2 PartsBothHistoryElem
                (JD.field "distance" JD.int)
                (JD.field "day" JD.int |> JD.map Time.millisToPosix)

        partsDecoder : JD.Decoder Parts
        partsDecoder =
            JD.field "type" JD.string
                |> JD.andThen
                    (\typeValue ->
                        case typeValue of
                            "both" ->
                                JD.map4 PartsBothArg
                                    (JD.field "name" JD.string)
                                    (JD.field "distance" JD.int)
                                    (JD.field "day" JD.int)
                                    (JD.field "history" (JD.dict historyElemDecoder))
                                    |> JD.map PartsBoth

                            "distance" ->
                                JD.map3 PartsDistanceArg
                                    (JD.field "name" JD.string)
                                    (JD.field "distance" JD.int)
                                    (JD.field "history" (JD.dict historyElemDecoder))
                                    |> JD.map PartsDistanceOnly

                            _ ->
                                JD.fail "invalid type"
                    )

        partsDictDecoder =
            JD.dict partsDecoder
    in
    JD.decodeString partsDictDecoder json


deleteParts : PartsKey -> PartsDict -> PartsDict
deleteParts =
    Dict.remove


getLatestHistory : PartsBothHistory -> Maybe PartsBothHistoryElem
getLatestHistory his =
    Dict.values his |> List.sortBy (\h -> h.day |> Time.posixToMillis |> negate) |> List.head


addNewHistory : HistoryKey -> PartsBothHistoryElem -> PartsBothHistory -> PartsBothHistory
addNewHistory =
    Dict.insert


mapHistory : (HistoryKey -> PartsBothHistoryElem -> a) -> PartsBothHistory -> List a
mapHistory f history =
    Dict.toList history
        |> List.sortBy (\( _, h ) -> h.day |> Time.posixToMillis |> negate)
        |> List.map (\( key, h ) -> f key h)


partsName : Parts -> String
partsName parts =
    case parts of
        PartsBoth arg ->
            arg.name

        PartsDistanceOnly arg ->
            arg.name


addDay : Day -> Date -> Date
addDay day date =
    Time.posixToMillis date + day |> Time.millisToPosix


toDay : Int -> Day
toDay day =
    day * 24 * 3600 * 1000


fromDay : Day -> Int
fromDay day =
    day // (24 * 3600 * 1000)


deleteHistory : Parts -> HistoryKey -> Parts
deleteHistory parts historyKey =
    case parts of
        PartsBoth e ->
            PartsBoth { e | history = Dict.remove historyKey e.history }

        PartsDistanceOnly e ->
            PartsDistanceOnly { e | history = Dict.remove historyKey e.history }
