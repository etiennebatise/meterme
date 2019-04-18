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
import Monocle.Common exposing (list)
import Monocle.Compose as Compose
import Monocle.Lens exposing (Lens)
import Monocle.Optional exposing (Optional)
import Platform exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Set exposing (..)
import String as Str


type alias Series =
    { reps : Int
    , weight : Int
    }


seriesReps : Lens Series Int
seriesReps =
    Lens .reps (\b a -> { a | reps = b })


seriesWeight : Lens Series Int
seriesWeight =
    Lens .weight (\b a -> { a | weight = b })


type alias Exercise =
    { name : String
    , series : List Series
    }


exerciseName : Lens Exercise String
exerciseName =
    Lens .name (\b a -> { a | name = b })


exerciseSeries : Lens Exercise (List Series)
exerciseSeries =
    Lens .series (\b a -> { a | series = b })


type alias Session =
    { date : String
    , workout : List Exercise
    }


sessionDate : Lens Session String
sessionDate =
    Lens .date (\b a -> { a | date = b })


sessionWorkout : Lens Session (List Exercise)
sessionWorkout =
    Lens .workout (\b a -> { a | workout = b })

sessionExerciseSeries : Int -> Int -> Optional Session Series
sessionExerciseSeries eIndex sIndex =
    sessionWorkout
        |> Compose.lensWithOptional (list eIndex)
        |> Compose.optionalWithLens exerciseSeries
        |> Compose.optionalWithOptional (list sIndex)



type SessionPart
    = Date
    | NewExercise
    | ExerciseName Int
    | SeriesReps Int Int
    | SeriesWeight Int Int


type Msg
    = NoOp
    | SubmitForm
    | AddExercise
    | UpdateSession SessionPart String
    | Uploaded (Result Http.Error ())


type alias Model =
    Session


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

        AddExercise ->
            let
                selected =
                    List.map .name model.workout

                newExercise =
                    MaybeE.unwrap [] (List.singleton << exercise) <| head <| remainingExercises selected

                m =
                    { model | workout = append model.workout newExercise }
            in
            ( m, Cmd.none )

        Uploaded _ ->
            ( { model | workout = [] }
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
            let
                name = sessionWorkout
                       |> Compose.lensWithOptional (list index)
                       |> Compose.optionalWithLens exerciseName
            in
                name.set value model

        SeriesReps eIndex sIndex ->
            let
                s = (sessionExerciseSeries eIndex sIndex) |> Compose.optionalWithLens seriesReps
                v = Maybe.withDefault 0 <| Str.toInt value
            in
                s.set v model

        SeriesWeight eIndex sIndex ->
            let
                s = (sessionExerciseSeries eIndex sIndex) |> Compose.optionalWithLens seriesWeight
                v = Maybe.withDefault 0 <| Str.toInt value
            in
                s.set v model

        NewExercise ->
            let
                selected =
                    List.map .name model.workout

                newExercise =
                    MaybeE.unwrap [] (List.singleton << initExercise) <| head <| remainingExercises selected

                m =
                    { model | workout = append model.workout newExercise }
            in
                sessionWorkout.set (append)


encodeModel : Model -> Encode.Value
encodeModel m =
    Encode.object
        [ ( "date", Encode.string m.date )
        , ( "workout", Encode.list encodeExercise m.workout )
        ]


encodeExercise : Exercise -> Encode.Value
encodeExercise e =
    Encode.object
        [ ( "name", Encode.string e.name )
        , ( "series", Encode.list encodeSeries e.series )
        ]


encodeSeries : Series -> Encode.Value
encodeSeries s =
    Encode.object
        [ ( "weight", Encode.int s.weight )
        , ( "reps", Encode.int s.reps )
        ]


initExercise : String -> Exercise
initExercise n =
    Exercise n <| repeat 4 series


series : Series
series =
    Series 0 0


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


viewSeries : Int -> Int -> Series -> Html Msg
viewSeries ei si s =
    let
        minReps =
            0

        maxReps =
            12
    in
    div []
        [ input
            [ type_ "number"
            , name "weight"
            , Attr.min "0"
            , value <| Str.fromInt s.weight
            , onInput <| UpdateSession (SeriesWeight ei si)
            ]
            []
        , input
            [ type_ "range"
            , name "reps"
            , Attr.max <| Str.fromInt maxReps
            , Attr.min <| Str.fromInt minReps
            , value <| Str.fromInt s.reps
            , onInput <| UpdateSession (SeriesReps ei si)
            ]
            []
        , text <| Str.fromInt s.reps
        ]


viewExercise : Int -> Exercise -> Html Msg
viewExercise i e =
    div
        []
    <|
        append
            [ select
                [ onInput <| UpdateSession (ExerciseName i)
                , value e.name
                ]
                (List.map (\x -> option [ value x, selected (x == e.name) ] [ text x ]) exerciseList)
            ]
            (indexedMap (viewSeries i) e.series)


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
            viewExercises model.workout

        modifierButton =
            if length model.workout == 4 then
                addSendButton

            else
                addExerciseButton

        formContent =
            [ div [] [ dateLabel ]
            , div [] exercises
            ]

        content =
            div
                []
                [ Html.form [ id "workout-form" ] formContent
                , div [] [ modifierButton ]
                ]
    in
    content


init : Model
init =
    { date = "", workout = [] }


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
