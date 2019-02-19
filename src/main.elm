module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, br, button, div, h1, input, option, select, text)
import Html.Attributes exposing (selected, type_, value)
import Html.Events exposing (onClick, onInput)
import List exposing (append, concat, indexedMap, intersperse, length, map, range)
import Maybe


type Msg
    = NoOp
    | CreateUnscheduledSession
    | ScheduleSession String


type Model
    = NoData
    | UnscheduledEmptySession
    | ScheduledEmptySession String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        CreateUnscheduledSession ->
            UnscheduledEmptySession

        ScheduleSession date ->
            ScheduledEmptySession date


datePicker : Maybe String -> Html Msg
datePicker default =
    input
        [ type_ "date"
        , onInput ScheduleSession
        , value <| Maybe.withDefault "" default
        ]
        []


view : Model -> Html Msg
view model =
    case model of
        NoData ->
            button [ onClick CreateUnscheduledSession ] [ text "Ajouter une session" ]

        UnscheduledEmptySession ->
            datePicker Nothing

        ScheduledEmptySession date ->
            div [] [ datePicker (Just date), button [] [ text "Foo" ] ]


init : Model
init =
    NoData


main =
    Browser.sandbox { init = init, update = update, view = view }
