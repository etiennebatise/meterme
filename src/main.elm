module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, br, button, div, h1, input, option, select, text, form, label)
import Html.Attributes exposing (selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List exposing (append, concat, indexedMap, intersperse, length, map, range)
import Maybe
import List.Extra exposing (updateAt)


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
    | SubmitForm
    | ScheduleSession String
    | UpdateExerciseName Int String


type alias Model =
    { date : String
    , session : List Exercise
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SubmitForm
            -> model

        ScheduleSession date ->
                    { model | date = date }

        UpdateExerciseName index name ->
                    { model | session = updateAt index (\e -> {e | name = name }) model.session}




datePicker : String -> Html Msg
datePicker date =
    input
        [ type_ "date"
        , onInput <| \d -> ScheduleSession d
        , value date
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
    form
        [ onSubmit SubmitForm ]
        [ label []
                [ datePicker model.date ]
        , label []
                [selectExercise]
        ]

init : Model
init = { date = "", session = [] }


main =
    Browser.sandbox { init = init, update = update, view = view }
