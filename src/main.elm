module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode
import List exposing (..)
import List.Extra as ListE
import Maybe as Maybe
import Maybe.Extra as MaybeE
import Monocle.Common as MCommon
import Monocle.Compose as MCompose
import Monocle.Lens as Lens exposing (Lens)
import Monocle.Optional as Optional exposing (Optional)
import Platform exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Set exposing (..)
import String as Str



-----------
-- TYPES --
-----------

type alias Weight = Float

strToWeight : String -> Weight
strToWeight s = Maybe.withDefault 0.0 <| Str.toFloat s

type alias Serie =
    { reps : Int
    , weight : Weight
    }


type alias Exercise =
    { name : String
    , series : List Serie
    }


type alias Session =
    { date : String
    , exercises : List Exercise
    }


type ExerciseIndex
    = ExerciseIndex Int


type SerieIndex
    = SerieIndex Int


type SessionPart
    = Date
    | NewExercise
    | ExerciseName ExerciseIndex
    | SerieReps ExerciseIndex SerieIndex
    | SerieWeight ExerciseIndex SerieIndex
    | NewSerie ExerciseIndex
    | RemoveSerie ExerciseIndex


type Msg
    = NoOp
    | SubmitForm
    | UpdateSession SessionPart String
    | Uploaded (Result Http.Error ())


type alias Model =
    Session



------------
-- UPDATE --
------------


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SubmitForm ->
            ( model
            , Http.post
                { url = "http://localhost:80/anything"
                , body = Http.jsonBody <| encodeModel model
                , expect = Http.expectWhatever Uploaded
                }
            )

        Uploaded _ ->
            ( { model | exercises = [] }
            , let
                a =
                    Debug.log "debug" model
              in
              Cmd.none
            )

        UpdateSession part value ->
            ( updateModel model part value, Cmd.none )


updateModel : Model -> SessionPart -> String -> Model
updateModel model part value =
    case part of
        Date ->
            sessionDate.set value model

        ExerciseName index ->
            (sessionExerciseName index).set value model

        SerieReps ei si ->
            (sessionExerciseSerieReps ei si).set (strToInt value) model

        SerieWeight ei si ->
            (sessionExerciseSerieWeight ei si).set (strToWeight value) model

        NewExercise ->
            let
                selected =
                    List.map .name model.exercises

                e =
                    MaybeE.unwrap [] (List.singleton << initExercise) <| head <| remainingExercises selected
            in
            Lens.modify sessionExercises (\l -> l ++ e) model

        NewSerie ei ->
            Optional.modify (sessionExerciseSeries ei) (\l -> l ++ [ initSerie ]) model

        RemoveSerie ei ->
            Optional.modify (sessionExerciseSeries ei) initL model



------------
-- LENSES --
-------------


serieReps : Lens Serie Int
serieReps =
    Lens .reps (\b a -> { a | reps = b })


serieWeight : Lens Serie Weight
serieWeight =
    Lens .weight (\b a -> { a | weight = b })


exerciseName : Lens Exercise String
exerciseName =
    Lens .name (\b a -> { a | name = b })


exerciseSeries : Lens Exercise (List Serie)
exerciseSeries =
    Lens .series (\b a -> { a | series = b })


sessionDate : Lens Session String
sessionDate =
    Lens .date (\b a -> { a | date = b })


sessionExercises : Lens Session (List Exercise)
sessionExercises =
    Lens .exercises (\b a -> { a | exercises = b })


sessionExerciseName : ExerciseIndex -> Optional Session String
sessionExerciseName (ExerciseIndex i) =
    sessionExercises
        |> MCompose.lensWithOptional (MCommon.list i)
        |> MCompose.optionalWithLens exerciseName


sessionExerciseSeries : ExerciseIndex -> Optional Session (List Serie)
sessionExerciseSeries (ExerciseIndex ei) =
    sessionExercises
        |> MCompose.lensWithOptional (MCommon.list ei)
        |> MCompose.optionalWithLens exerciseSeries


sessionExerciseSerie : ExerciseIndex -> SerieIndex -> Optional Session Serie
sessionExerciseSerie ei (SerieIndex si) =
    sessionExerciseSeries ei
        |> MCompose.optionalWithOptional (MCommon.list si)


sessionExerciseSerieReps : ExerciseIndex -> SerieIndex -> Optional Session Int
sessionExerciseSerieReps ei si =
    sessionExerciseSerie ei si |> MCompose.optionalWithLens serieReps


sessionExerciseSerieWeight : ExerciseIndex -> SerieIndex -> Optional Session Weight
sessionExerciseSerieWeight ei si =
    sessionExerciseSerie ei si |> MCompose.optionalWithLens serieWeight



----------
-- JSON --
----------


encodeModel : Model -> Encode.Value
encodeModel m =
    Encode.object
        [ ( "date", Encode.string m.date )
        , ( "exercises", Encode.list encodeExercise m.exercises )
        ]


encodeExercise : Exercise -> Encode.Value
encodeExercise e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "series", Encode.list encodeSerie e.series )
        ]


encodeSerie : Serie -> Encode.Value
encodeSerie s =
    Encode.object
        [ ( "weight", Encode.float s.weight )
        , ( "reps", Encode.int s.reps )
        ]


strToInt : String -> Int
strToInt s =
    Maybe.withDefault 0 <| Str.toInt s



-------------
-- HELPERS --
-------------


defaultNumberOfSeries : Int
defaultNumberOfSeries =
    4


initExercise : String -> Exercise
initExercise n =
    Exercise n <| repeat defaultNumberOfSeries initSerie


initSerie : Serie
initSerie =
    Serie 0 0


initL : List a -> List a
initL l =
    Maybe.withDefault [] <| ListE.init l



----------
-- HTML --
----------


datePicker : String -> Html Msg
datePicker date =
    input
        [ type_ "date"
        , onInput <| \d -> UpdateSession Date d
        , value date
        ]
        []


exerciseList : List String
exerciseList =
    [ "squat", "pull_up", "bench_press", "overhead_press" ]


remainingExercises : List String -> List String
remainingExercises l =
    toList <| diff (fromList exerciseList) (fromList l)


viewSerie : ExerciseIndex -> Int -> Serie -> Html Msg
viewSerie ei i s =
    let
        minReps =
            0

        maxReps =
            12

        si =
            SerieIndex i

        weightInput =
            input
                [ type_ "number"
                , name "weight"
                , Attr.min "0"
                , step "0.5"
                , value <| Str.fromFloat s.weight
                , onInput <| UpdateSession (SerieWeight ei si)
                ]
                []

        repsInput =
            input
                [ type_ "range"
                , name "reps"
                , Attr.max <| Str.fromInt maxReps
                , Attr.min <| Str.fromInt minReps
                , value <| Str.fromInt s.reps
                , onInput <| UpdateSession (SerieReps ei si)
                ]
                []
    in
    div []
        [ weightInput
        , repsInput
        , text <| Str.fromInt s.reps
        ]


viewExercise : Int -> Exercise -> Html Msg
viewExercise i e =
    let
        ei =
            ExerciseIndex i

        nameSelector =
            select
                [ onInput <| UpdateSession (ExerciseName ei)
                , value e.name
                ]
                (List.map (\x -> option [ value x, selected (x == e.name) ] [ text x ]) exerciseList)

        series =
            indexedMap (viewSerie ei) e.series

        addSerieButton =
            button [ onClick <| UpdateSession (NewSerie ei) "" ] [ text "add series" ]

        removeSerieButton =
            button [ onClick <| UpdateSession (RemoveSerie ei) "" ] [ text "remove series" ]
    in
    div
        []
    <|
        [ nameSelector, addSerieButton, removeSerieButton ]
            ++ series


addExerciseButton : Html Msg
addExerciseButton =
    button [ onClick <| UpdateSession NewExercise "" ] [ text "+" ]


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
            viewExercises model.exercises

        modifierButton =
            if length model.exercises == length exerciseList then
                addSendButton

            else
                addExerciseButton
    in
    div
        []
        [ div [] [ dateLabel ]
        , div [] exercises
        , div [] [ modifierButton ]
        ]


init : Model
init =
    { date = "", exercises = [] }


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
