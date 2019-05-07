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
import Monocle.Lens as Lens
import Monocle.Lens exposing (Lens)
import Monocle.Optional as Optional
import Monocle.Optional exposing (Optional)
import Platform exposing (..)
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Set exposing (..)
import String as Str



-----------
-- TYPES --
-----------


type alias Serie =
    { reps : Int
    , weight : Int
    }


type alias Exercise =
    { name : String
    , series : List Serie
    }


type alias Session =
    { date : String
    , exercises : List Exercise
    }


type alias ExerciseIndex =
    Int


type alias SerieIndex =
    Int


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
            let
                name =
                    sessionExercises
                        |> MCompose.lensWithOptional (MCommon.list index)
                        |> MCompose.optionalWithLens exerciseName
            in
            name.set value model

        SerieReps eIndex sIndex ->
            let
                s =
                    sessionExerciseSerie eIndex sIndex |> MCompose.optionalWithLens serieReps

                v =
                    Maybe.withDefault 0 <| Str.toInt value
            in
            s.set v model

        SerieWeight eIndex sIndex ->
            let
                s =
                    sessionExerciseSerie eIndex sIndex |> MCompose.optionalWithLens serieWeight

                v =
                    Maybe.withDefault 0 <| Str.toInt value
            in
            s.set v model

        NewExercise ->
            let
                selected =
                    List.map .name model.exercises

                e =
                    MaybeE.unwrap [] (List.singleton << initExercise) <| head <| remainingExercises selected
            in
                Lens.modify sessionExercises (\l -> l ++ e) model

        NewSerie eIndex ->
            let
                s =
                    sessionExercises
                        |> MCompose.lensWithOptional (MCommon.list eIndex)
                        |> MCompose.optionalWithLens exerciseSeries
            in
            Optional.modify s (\l -> l ++ [ initSerie ]) model

        RemoveSerie eIndex ->
            let
                s =
                    sessionExercises
                        |> MCompose.lensWithOptional (MCommon.list eIndex)
                        |> MCompose.optionalWithLens exerciseSeries
            in
            Optional.modify s initL model



------------
-- LENSES --
-------------


serieReps : Lens Serie Int
serieReps =
    Lens .reps (\b a -> { a | reps = b })


serieWeight : Lens Serie Int
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


sessionExerciseSerie : Int -> Int -> Optional Session Serie
sessionExerciseSerie eIndex sIndex =
    sessionExercises
        |> MCompose.lensWithOptional (MCommon.list eIndex)
        |> MCompose.optionalWithLens exerciseSeries
        |> MCompose.optionalWithOptional (MCommon.list sIndex)



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
        [ ( "weight", Encode.int s.weight )
        , ( "reps", Encode.int s.reps )
        ]



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


viewSerie : Int -> Int -> Serie -> Html Msg
viewSerie ei si s =
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
            , onInput <| UpdateSession (SerieWeight ei si)
            ]
            []
        , input
            [ type_ "range"
            , name "reps"
            , Attr.max <| Str.fromInt maxReps
            , Attr.min <| Str.fromInt minReps
            , value <| Str.fromInt s.reps
            , onInput <| UpdateSession (SerieReps ei si)
            ]
            []
        , text <| Str.fromInt s.reps
        ]


viewExercise : Int -> Exercise -> Html Msg
viewExercise i e =
    let
        nameSelector =
            select
                [ onInput <| UpdateSession (ExerciseName i)
                , value e.name
                ]
                (List.map (\x -> option [ value x, selected (x == e.name) ] [ text x ]) exerciseList)

        series =
            indexedMap (viewSerie i) e.series

        addSerieButton =
            button [ onClick <| UpdateSession (NewSerie i) "" ] [ text "add series" ]

        removeSerieButton =
            button [ onClick <| UpdateSession (RemoveSerie i) "" ] [ text "remove series" ]
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
