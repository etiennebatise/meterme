module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, br, button, div, h1, input, option, select, text)
import Html.Attributes exposing (selected, type_, value)
import Html.Events exposing (onClick, onInput)
import List exposing (append, concat, indexedMap, intersperse, length, map, range)
import Maybe


type alias Series =
    { numberOfReps : Int
    , weight : Int
    }



type alias Exercise =
    { name : String
    , training : List Series
    }


type Msg
    = NoOp
    | CreateUnscheduledSession
    | ScheduleSession String
    | UpdateExerciseName Int String


type Model
    = NoData
    | UnscheduledEmptySession
    | ScheduledSession String Exercise


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        CreateUnscheduledSession ->
            UnscheduledEmptySession

        ScheduleSession date ->
            case model of
                NoData -> NoData
                UnscheduledEmptySession ->
                    ScheduledSession date (Exercise "squat" [])
                ScheduledSession _ l -> ScheduledSession date l

        UpdateExerciseName index name ->
            case model of
                NoData -> NoData
                UnscheduledEmptySession -> model
                ScheduledSession d l ->
                   ScheduledSession d (Exercise name [])




datePicker : Maybe String -> Html Msg
datePicker default =
    input
        [ type_ "date"
        , onInput <| \d -> ScheduleSession d
        , value <| Maybe.withDefault "" default
        ]
        []


selectExercise : Html Msg
selectExercise =
    select
        [ onInput <| UpdateExerciseName 0 ]
        [ option [ value "squat" ] [ text "Squats" ]
        , option [ value "pullup" ] [ text "Pull-ups" ]
        ]


view : Model -> Html Msg
view model =
    case model of
        NoData ->
            button [ onClick CreateUnscheduledSession ] [ text "Ajouter une session" ]

        UnscheduledEmptySession ->
            datePicker Nothing

        ScheduledSession date l ->
            div []
                [ datePicker (Just date)
                , br [] []
                , selectExercise
                ]


init : Model
init =
    NoData


main =
    Browser.sandbox { init = init, update = update, view = view }
