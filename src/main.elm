module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Attribute, Html, br, button, div, form, h1, input, label, option, select, text, datalist)
import Html.Attributes exposing (name, selected, type_, value, max, min, step, list, for, id)
import Html.Events exposing (onClick, onInput, onSubmit)
import List exposing (append, concat, drop, head, indexedMap, intersperse, length, map, range, repeat, reverse, singleton, take)
import List.Extra as ListE
import Maybe as Maybe
import Maybe.Extra as MaybeE
import Set exposing (diff, fromList, toList)
import String as Str


type alias Series =
    { reps : Int
    , weight : Int
    }


type alias Exercise =
    { name : String
    , series : List Series
    }


type Msg
    = NoOp
    | SubmitForm
    | AddExercise
    | ScheduleSession String
    | UpdateExerciseName Int String
    | UpdateExerciseSeriesReps Int Int String
    | UpdateExerciseSeriesWeight Int Int String


type alias Model =
    { date : String
    , session : List Exercise
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        SubmitForm ->
            model

        ScheduleSession date ->
            { model | date = date }

        AddExercise ->
            let
                selected =
                    map .name model.session

                newExercise =
                    MaybeE.unwrap [] (singleton << exercise) <| head <| remainingExercises selected
            in
            { model | session = append model.session newExercise }

        UpdateExerciseName index name ->
            { model | session = ListE.updateAt index (\e -> { e | name = name }) model.session }

        UpdateExerciseSeriesReps ei si v ->
            { model | session = ListE.updateAt ei (\e -> { e | series = ListE.updateAt si (\s -> { s | reps = Maybe.withDefault 0 <| Str.toInt v }) e.series }) model.session }

        UpdateExerciseSeriesWeight ei si v ->
            { model | session = ListE.updateAt ei (\e -> { e | series = ListE.updateAt si (\s -> { s | weight = Maybe.withDefault 0 <| Str.toInt v }) e.series }) model.session }


exercise : String -> Exercise
exercise n =
    Exercise n <| repeat 4 series


series : Series
series =
    Series 0 0


datePicker : String -> Html Msg
datePicker date =
    input
        [ type_ "date"
        , onInput <| \d -> ScheduleSession d
        , value date
        ]
        []


exerciseList : List String
exerciseList =
    [ "squat", "pull_up", "bench_press", "overhead_press" ]


remainingExercises : List String -> List String
remainingExercises l =
    toList <| diff (fromList exerciseList) (fromList l)

viewSeries : Int -> Int -> Series -> Html Msg
viewSeries ei si s =
    let
        minReps = 0
        maxReps = 12
    in
        div []
            [ input [ type_ "range", name "reps", max <| Str.fromInt maxReps, min <| Str.fromInt minReps, value <| Str.fromInt s.reps, onInput <| UpdateExerciseSeriesReps ei si ] []
            , text <| Str.fromInt s.reps
            , input [ type_ "number", name "weight", min "0", value <| Str.fromInt s.weight, onInput <| UpdateExerciseSeriesWeight ei si ] []
            ]


viewExercise : Int -> Exercise -> Html Msg
viewExercise i e =
    div
        []
    <|
        append
            [ select
                [ onInput <| UpdateExerciseName i
                , value e.name
                ]
                (map (\x -> option [ value x, selected (x == e.name) ] [ text x ]) exerciseList)
            ]
            (indexedMap (viewSeries i) e.series)


addExerciseButton : Html Msg
addExerciseButton =
    button [ onClick AddExercise ] [ text "+" ]


viewExercises : List Exercise -> List (Html Msg)
viewExercises exs =
    let
        names =
            map .name exs

        remaining i =
            remainingExercises <| take i names
    in
    indexedMap viewExercise exs


view : Model -> Html Msg
view model =
    let
        dateLabel =
            label [] [ datePicker model.date ]

        exercises =
            viewExercises model.session

        content =
            intersperse (br [] []) <| append [ dateLabel ] <| append exercises [ addExerciseButton ]
    in
    form
        [ onSubmit SubmitForm ]
        content


init : Model
init =
    { date = "", session = [] }


main =
    Browser.sandbox { init = init, update = update, view = view }
