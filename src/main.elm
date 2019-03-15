module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, br, button, div, h1, input, option, select, text, form, label)
import Html.Attributes exposing (selected, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import List exposing (append, concat, indexedMap, intersperse, length, map, range)
import Set exposing (fromList, toList, diff)
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
    | AddExercise String
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

        AddExercise name ->
                    { model | session = append model.session [Exercise name []]}

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

exerciseList : List String
exerciseList = ["squat", "pull_up", "bench_press", "overhead_press"]

selectExercise : Int -> Exercise -> Html Msg
selectExercise i e =
    select
        [ onInput <| UpdateExerciseName i
        , value e.name ]
        (map (\x -> option [value x, selected (x == e.name)] [text x]) exerciseList)

addExerciseButton : List Exercise -> Html Msg
addExerciseButton s =
    let alreadySelected =  fromList <| map (\e -> e.name) s
        remaining = diff (fromList ["squat", "pull_up", "bench_press", "overhead_press"]) alreadySelected
    in
        select
            [ onInput AddExercise ]
            (map (\x -> option [value x] [text x]) <| toList remaining)

view : Model -> Html Msg
view model =
    let dateLabel = label [] [ datePicker model.date ]
        exercises = indexedMap (\i e -> label [] [selectExercise i e]) model.session
        addNew = addExerciseButton model.session
        content = append [ dateLabel ] <| append exercises [ br [] [],  addNew ]
    in
      form
          [ onSubmit SubmitForm ]
          content

init : Model
init = { date = "", session = [] }


main =
    Browser.sandbox { init = init, update = update, view = view }
