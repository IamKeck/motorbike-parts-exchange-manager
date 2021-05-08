port module Main exposing (main)

import Browser
import Dict
import Form.Decoder as FD
import Html exposing (..)
import Html.Attributes as HA
import Html.Events as HE
import NewPartsForm
import PartsLogForm
import Random
import Task
import Time
import Types
import Util
import View exposing (onInputAsNumber)


type BottomTab
    = NewParts
    | PartsLog


type alias Model =
    { parts : Types.PartsDict
    , form : NewPartsForm.InputForm
    , partsLogForms : Dict.Dict Types.PartsKey PartsLogForm.PartsLogForm
    , distance : Maybe Int
    , now : Maybe Time.Posix
    , selectedParts : Maybe Types.PartsKey
    , currentTab : BottomTab
    }


type Msg
    = NoOp
    | FormTypeChanged NewPartsForm.PartsType
    | FormDistanceInput String
    | FormDayInput String
    | FormNameInput String
    | RegisterNewParts
    | NewPartsKey Types.Parts Types.PartsKey
    | PartsLogDateInput Types.PartsKey Int
    | PartsLogDistanceInput Types.PartsKey String
    | PartsLogRegister Types.PartsKey
    | DeleteParts Types.PartsKey
    | DistanceInput String
    | GotTime Time.Posix
    | GotPartsLogKey Types.PartsKey Types.PartsBothHistoryElem Types.HistoryKey
    | SelectPartsTab Types.PartsKey
    | SelectBottomTab BottomTab


type alias Flags =
    String


port savePartsDict : String -> Cmd msg


init : Flags -> ( Model, Cmd Msg )
init savedDictJson =
    let
        initialModel =
            { parts = Dict.empty
            , partsLogForms = Dict.empty
            , distance = Nothing
            , now = Nothing
            , selectedParts = Nothing
            , currentTab = PartsLog
            , form =
                { name = ""
                , type_ = NewPartsForm.Both
                , distance = ""
                , day = ""
                }
            }

        getNow =
            Task.perform GotTime Time.now
    in
    case Types.restoreParts savedDictJson of
        Err e ->
            Debug.log (Debug.toString e) ( initialModel, getNow )

        Ok p ->
            let
                firstKey =
                    Dict.toList p |> List.head |> Maybe.map Tuple.first
            in
            ( { initialModel | parts = p, selectedParts = firstKey }, getNow )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FormTypeChanged partsType ->
            let
                oldForm =
                    model.form

                newForm =
                    { oldForm | type_ = partsType }
            in
            ( { model | form = newForm }, Cmd.none )

        FormDistanceInput newDistance ->
            let
                oldForm =
                    model.form

                newForm =
                    { oldForm | distance = newDistance }
            in
            ( { model | form = newForm }, Cmd.none )

        FormDayInput newDay ->
            let
                oldForm =
                    model.form

                newForm =
                    { oldForm | day = newDay }
            in
            ( { model | form = newForm }, Cmd.none )

        FormNameInput newName ->
            let
                oldForm =
                    model.form

                newForm =
                    { oldForm | name = newName }
            in
            ( { model | form = newForm }, Cmd.none )

        RegisterNewParts ->
            case FD.run NewPartsForm.formDecoder model.form of
                Err e ->
                    Debug.log (Debug.toString e) ( model, Cmd.none )

                Ok parts ->
                    ( model, generatePartsKey parts )

        NewPartsKey parts key ->
            let
                newParts =
                    Types.addNewPartsToDict key parts model.parts
            in
            ( { model | parts = newParts }, savePartsDict <| Types.saveParts newParts )

        PartsLogDateInput key day ->
            let
                form =
                    Dict.get key model.partsLogForms
                        |> Maybe.withDefault PartsLogForm.partsLogFormDefault

                newForm =
                    { form | day = Just day }

                newForms =
                    Dict.insert key newForm model.partsLogForms
            in
            ( { model | partsLogForms = newForms }, Cmd.none )

        PartsLogDistanceInput key distance ->
            let
                form =
                    Dict.get key model.partsLogForms
                        |> Maybe.withDefault PartsLogForm.partsLogFormDefault

                newForm =
                    { form | distance = Just distance }

                newForms =
                    Dict.insert key newForm model.partsLogForms
            in
            ( { model | partsLogForms = newForms }, Cmd.none )

        PartsLogRegister partsKey ->
            let
                form =
                    Dict.get partsKey model.partsLogForms
            in
            case form of
                Nothing ->
                    Debug.log "nothing" ( model, Cmd.none )

                Just f ->
                    case FD.run PartsLogForm.partsLogFormDecoder f of
                        Err e ->
                            Debug.log (Debug.toString e) ( model, Cmd.none )

                        Ok l ->
                            ( model
                            , generateHistoryKey partsKey l
                            )

        GotPartsLogKey partsKey historyElem historyKey ->
            let
                newDict =
                    Types.addNewLogToDict partsKey model.parts historyKey historyElem
            in
            ( { model | parts = newDict }
            , savePartsDict <| Types.saveParts newDict
            )

        DeleteParts key ->
            let
                newDict =
                    Types.deleteParts key model.parts
            in
            ( { model | parts = newDict }, savePartsDict <| Types.saveParts newDict )

        DistanceInput km ->
            ( { model | distance = String.toInt km }, Cmd.none )

        GotTime time ->
            ( { model | now = Just time }, Cmd.none )

        SelectPartsTab key ->
            ( { model | selectedParts = Just key }, Cmd.none )

        SelectBottomTab tab ->
            ( { model | currentTab = tab }, Cmd.none )


generatePartsKey : Types.Parts -> Cmd Msg
generatePartsKey parts =
    Types.generatePartsKey |> Random.generate (NewPartsKey parts)


generateHistoryKey : Types.PartsKey -> Types.PartsBothHistoryElem -> Cmd Msg
generateHistoryKey partsKey partsElem =
    Types.generateHistoryKey |> Random.generate (GotPartsLogKey partsKey partsElem)


view : Model -> Html Msg
view model =
    div [ HA.class "mainContainer" ]
        [ section [ HA.class "hero is-primary is-small headerSection" ]
            [ div [ HA.class "hero-body" ]
                [ p [ HA.class "title" ]
                    [ text "パーツ管理" ]
                ]
            ]
        , section [ HA.class "contentSection" ]
            [ div []
                [ text "現在の走行距離"
                , input [ HE.onInput DistanceInput ] []
                , text "km"
                ]
            , case model.currentTab of
                NewParts ->
                    inputFormView model.form

                PartsLog ->
                    case model.selectedParts of
                        Nothing ->
                            text ""

                        Just selectedParts ->
                            div []
                                [ partsTab selectedParts model.parts
                                , partsView model.distance model.now model.selectedParts model.parts
                                ]
            ]
        , section [ HA.class "footerSection" ] [ bottomTabView model.currentTab ]
        ]


inputFormView : NewPartsForm.InputForm -> Html Msg
inputFormView inputForm =
    div []
        [ div [ HA.class "field" ]
            [ label [ HA.class "label" ] [ text "タイプ" ]
            , div [ HA.class "control" ]
                [ label [ HA.class "radio" ]
                    [ input
                        [ HA.type_ "radio"
                        , HA.name "type"
                        , HE.onClick <| FormTypeChanged NewPartsForm.Both
                        , HA.checked <| inputForm.type_ == NewPartsForm.Both
                        ]
                        []
                    , text "走行距離&日数経過"
                    ]
                , label [ HA.class "radio" ]
                    [ input
                        [ HA.type_ "radio"
                        , HA.name "type"
                        , HE.onClick <| FormTypeChanged NewPartsForm.Distance
                        , HA.checked <| inputForm.type_ == NewPartsForm.Distance
                        ]
                        []
                    , text "走行距離"
                    ]
                ]
            ]
        , div [ HA.class "field" ]
            [ label [ HA.class "label" ] [ text "距離" ]
            , div [ HA.class "control" ] [ input [ HA.type_ "number", HE.onInput FormDistanceInput ] [] ]
            ]
        , if inputForm.type_ /= NewPartsForm.Distance then
            div [ HA.class "field" ] [ label [ HA.class "label" ] [ text "日数" ], div [ HA.class "control" ] [ input [ HA.type_ "number", HE.onInput FormDayInput ] [] ] ]

          else
            text ""
        , div [ HA.class "field" ]
            [ label [ HA.class "label" ] [ text "パーツ名" ]
            , div [ HA.class "control" ] [ input [ HA.type_ "text", HE.onInput FormNameInput ] [] ]
            ]
        , div [ HA.class "field" ]
            [ div [ HA.class "control" ]
                [ button [ HE.onClick RegisterNewParts, HA.class "button is-link" ] [ text "パーツ登録" ]
                ]
            ]
        ]


partsInputForm : Types.PartsKey -> Html Msg
partsInputForm key =
    div []
        [ div [ HA.class "field" ]
            [ label [ HA.class "label" ] [ text "日付" ]
            , div [ HA.class "control" ]
                [ input
                    [ HA.class "input", HA.type_ "date", onInputAsNumber (PartsLogDateInput key) ]
                    []
                ]
            ]
        , div [ HA.class "field" ]
            [ label [ HA.class "label" ] [ text "距離" ]
            , div [ HA.class "control" ]
                [ input
                    [ HA.class "input", HA.type_ "number", HE.onInput (PartsLogDistanceInput key) ]
                    []
                ]
            ]
        , div [ HA.class "field is-grouped" ]
            [ div [ HA.class "control" ] [ button [ HA.class "button is-link", HE.onClick <| PartsLogRegister key ] [ text "登録" ] ]
            , div [ HA.class "control" ] [ button [ HA.class "button is-link is-light", HE.onClick <| DeleteParts key ] [ text "パーツ削除" ] ]
            ]
        ]


partsLogList : Types.PartsBothHistory -> Html Msg
partsLogList hist =
    table [ HA.class "table" ]
        [ thead []
            [ tr []
                [ th [] [ text "日付" ]
                , th [] [ text "走行距離" ]
                ]
            ]
        , tbody [] <|
            Types.mapHistory
                (\_ his ->
                    tr []
                        [ td [] [ text <| Types.showDate <| his.day ]
                        , td [] [ text <| Util.withSuffix "km" <| his.distance ]
                        ]
                )
                hist
        ]


partsView : Maybe Int -> Maybe Time.Posix -> Maybe Types.PartsKey -> Types.PartsDict -> Html Msg
partsView km now mayKey partsDict =
    let
        keyToParts key =
            Dict.get key partsDict |> Maybe.map (\parts -> ( key, parts ))
    in
    case mayKey |> Maybe.andThen keyToParts of
        Nothing ->
            text ""

        Just ( key, parts ) ->
            case parts of
                Types.PartsBoth arg ->
                    div []
                        [ partsAlertView parts km now
                        , partsInputForm key
                        , p []
                            [ text <|
                                String.fromInt arg.distance
                                    ++ "kmもしくは"
                                    ++ String.fromInt arg.day
                                    ++ "日ごとに交換"
                            ]
                        , partsLogList arg.history
                        ]

                Types.PartsDistanceOnly arg ->
                    div []
                        [ h1 [] [ text arg.name ]
                        , partsInputForm key
                        , p []
                            [ text <|
                                String.fromInt arg.distance
                                    ++ "kmごとに交換"
                            ]
                        , partsLogList arg.history
                        ]


partsAlertView : Types.Parts -> Maybe Int -> Maybe Time.Posix -> Html Msg
partsAlertView parts km now =
    case now of
        Nothing ->
            text ""

        Just now_ ->
            case Types.partsStatus (Maybe.withDefault 0 km) now_ parts of
                Types.Ok ->
                    div [] [ text "交換不要" ]

                Types.ReplaceRequired ->
                    div [] [ text "要交換" ]

                Types.AlmostReplaceRequiredDistance dist ->
                    div [] [ text <| "あと" ++ String.fromInt dist ++ "kmで交換" ]

                Types.AlmostReplaceRequiredDay day ->
                    div [] [ text <| "あと" ++ String.fromInt day ++ "日で交換" ]


partsTab : Types.PartsKey -> Types.PartsDict -> Html Msg
partsTab selectedKey dict =
    div [ HA.class "tabs" ]
        [ ul [] <|
            List.map
                (\( key, parts ) ->
                    li [ HA.classList [ ( "is-active", selectedKey == key ) ] ]
                        [ a [ HE.onClick <| SelectPartsTab key ]
                            [ text <| Types.partsName parts ]
                        ]
                )
                (Dict.toList dict)
        ]


bottomTabView : BottomTab -> Html Msg
bottomTabView currentTab =
    div [ HA.class "tabs is-boxed" ]
        [ ul []
            [ li [ HA.classList [ ( "is-active", currentTab == PartsLog ) ] ]
                [ a [ HE.onClick <| SelectBottomTab PartsLog ] [ text "log" ] ]
            , li [ HA.classList [ ( "is-active", currentTab == NewParts ) ] ]
                [ a [ HE.onClick <| SelectBottomTab NewParts ] [ text "new parts" ] ]
            ]
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
