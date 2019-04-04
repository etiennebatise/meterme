module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import List exposing (..)
import List.Extra as ListE
import Maybe as Maybe
import Maybe.Extra as MaybeE
import Platform exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Set exposing (..)
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

    

update : Msg -> Model -> (Model, Cmd.Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

        SubmitForm ->
            (model, Cmd.none)

        ScheduleSession date ->
            let
                m = { model | date = date }
            in
                (m, Cmd.none)

        AddExercise ->
            let
                selected =
                    List.map .name model.session

                newExercise =
                    MaybeE.unwrap [] (List.singleton << exercise) <| head <| remainingExercises selected
                m = { model | session = append model.session newExercise }
            in
                (m, Cmd.none)

        UpdateExerciseName index name ->
            let
                m = { model | session = ListE.updateAt index (\e -> { e | name = name }) model.session }
            in
                (m, Cmd.none)

        UpdateExerciseSeriesReps ei si v ->
            let
                m = { model | session = ListE.updateAt ei (\e -> { e | series = ListE.updateAt si (\s -> { s | reps = Maybe.withDefault 0 <| Str.toInt v }) e.series }) model.session }
            in
                (m, Cmd.none)

        UpdateExerciseSeriesWeight ei si v ->
            let
                m = { model | session = ListE.updateAt ei (\e -> { e | series = ListE.updateAt si (\s -> { s | weight = Maybe.withDefault 0 <| Str.toInt v }) e.series }) model.session }
            in
                (m, Cmd.none)


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
            [ input
                  [ type_ "range"
                  , name "reps"
                  , Attr.max <| Str.fromInt maxReps
                  , Attr.min <| Str.fromInt minReps
                  , value <| Str.fromInt s.reps
                  , onInput <| UpdateExerciseSeriesReps ei si ]
                  []
            , text <| Str.fromInt s.reps
            , input
                  [ type_ "number"
                  , name "weight"
                  , Attr.min "0"
                  , value <| Str.fromInt s.weight
                  , onInput <| UpdateExerciseSeriesWeight ei si ] []
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
                (List.map (\x -> option [ value x, selected (x == e.name) ] [ text x ]) exerciseList)
            ]
            (indexedMap (viewSeries i) e.series)


addExerciseButton : Html Msg
addExerciseButton =
    button [ onClick AddExercise ] [ text "+" ]

addSendButton : Html Msg
addSendButton =
    button [ onClick SubmitForm ] [ text "OK" ]


viewExercises : List Exercise -> List (Html Msg)
viewExercises exs =
    let
        names =
            List.map .name exs

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

        modifierButton =
            if length model.session == 4
              then addSendButton
              else addExerciseButton

        content =
            [ div [] [ dateLabel ]
            , div [] exercises
            , div [] [ modifierButton ]
            ]
    in
    Html.form
        [ id "workout-form", onSubmit SubmitForm ]
        content


init : Model
init =
    { date = "", session = [] }


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> (init, Cmd.none)
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view }
