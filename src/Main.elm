module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Bulma.CDN exposing (..)
import Bulma.Elements exposing (table, tableModifiers, tableRow, tableCell, button, buttonModifiers)
import Bulma.Form exposing (controlSelectModifiers, controlSelect, controlInput, controlInputModifiers)
import Debug
import Html exposing (Html, div, input, option, p, select, text)
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
import Result exposing (..)
import Set exposing (..)
import String as Str
import Tuple exposing (..)
import Validate



-----------
-- TYPES --
-----------


type alias Weight =
    Float


strToWeight : String -> Weight
strToWeight s =
    Maybe.withDefault 0.0 <| Str.toFloat s


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


type alias Error =
    ( SessionPart, String )


type Msg
    = NoOp
    | SubmitForm
    | UpdateSession SessionPart String
    | Uploaded (Result Http.Error ())


type alias Model =
    { history : List Session
    , session : Session
    , errors : List Error
    }



------------
-- UPDATE --
------------


update : Msg -> Model -> ( Model, Cmd.Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SubmitForm ->
            case Validate.validate sessionValidator model.session of
                Ok validSession ->
                    ( { model | errors = [] }
                    , Http.post
                        { url = "http://localhost:80/anything"
                        , body = Http.jsonBody <| encodeSession <| Validate.fromValid validSession
                        , expect = Http.expectWhatever Uploaded
                        }
                    )

                Err validationErrors ->
                    ( { model | errors = validationErrors }, Cmd.none )

        Uploaded _ ->
            ( { model | history = model.history ++ [ model.session ], session = Session "" [] }
            , let
                a =
                    Debug.log "debug" model
              in
              Cmd.none
            )

        UpdateSession part value ->
            ( { model | session = updateSession model.session part value }, Cmd.none )


updateSession : Session -> SessionPart -> String -> Session
updateSession session part value =
    case part of
        Date ->
            sessionDate.set value session

        ExerciseName index ->
            (sessionExerciseName index).set value session

        SerieReps ei si ->
            (sessionExerciseSerieReps ei si).set (strToInt value) session

        SerieWeight ei si ->
            (sessionExerciseSerieWeight ei si).set (strToWeight value) session

        NewExercise ->
            let
                selected =
                    List.map .name session.exercises

                e =
                    MaybeE.unwrap [] (List.singleton << initExercise) <| head <| remainingExercises selected
            in
            Lens.modify sessionExercises (\l -> l ++ e) session

        NewSerie ei ->
            Optional.modify (sessionExerciseSeries ei) (\l -> l ++ [ initSerie ]) session

        RemoveSerie ei ->
            Optional.modify (sessionExerciseSeries ei) initL session



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


encodeSession : Session -> Encode.Value
encodeSession s =
    Encode.object
        [ ( "date", Encode.string s.date )
        , ( "exercises", Encode.list encodeExercise s.exercises )
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



----------------
-- VALIDATION --
----------------


sessionValidator : Validate.Validator Error Session
sessionValidator =
    Validate.all
        [ Validate.ifBlank .date ( Date, "Date can't be blank." )
        ]



----------
-- HTML --
----------


viewSessionErrors : List Error -> Html Msg
viewSessionErrors l =
    div [] (List.map (\( e, s ) -> p [] <| List.singleton <| text s) l)


datePicker : String -> Html Msg
datePicker date =
    controlInput
        controlInputModifiers
        []
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


viewSerieInput : ExerciseIndex -> Int -> Serie -> Html Msg
viewSerieInput ei i s =
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


viewExerciseInput : Int -> Exercise -> Html Msg
viewExerciseInput i e =
    let
        ei =
            ExerciseIndex i

        nameSelector =
            controlSelect
                controlSelectModifiers
                []
                [ onInput <| UpdateSession (ExerciseName ei)
                , value e.name
                ]
                (List.map (\x -> option [ value x, selected (x == e.name) ] [ text x ]) exerciseList)

        series =
            indexedMap (viewSerieInput ei) e.series

        addSerieButton =
            button buttonModifiers [ onClick <| UpdateSession (NewSerie ei) "" ] [ text "add series" ]

        removeSerieButton =
            button buttonModifiers [ onClick <| UpdateSession (RemoveSerie ei) "" ] [ text "remove series" ]
    in
    div
        []
    <|
        [ nameSelector, addSerieButton, removeSerieButton ]
            ++ series


addExerciseButton : Html Msg
addExerciseButton =
    button buttonModifiers [ onClick <| UpdateSession NewExercise "" ] [ text "+" ]


addSendButton : Html Msg
addSendButton =
    button buttonModifiers [ onClick SubmitForm ] [ text "OK" ]


viewExercisesInput : List Exercise -> List (Html Msg)
viewExercisesInput exs =
    let
        names =
            List.map .name exs

        remaining i =
            remainingExercises <| take i names
    in
    indexedMap viewExerciseInput exs


viewHistory : List Session -> Html Msg
viewHistory l =
    let
        elem =
            div
                [ id "history" ]
                (List.map viewSession l)

        mods = { bordered = False
               , striped = True
               , narrow = False
               , hoverable = True
               , fullWidth = True
               }
    in
    table mods [ id "history" ] (List.map viewSession l)


viewSession : Session -> Html Msg
viewSession s =
    let attrs = [ class "session-history" ]
        cells = [ tableCell [] [ text s.date ]
                ]
                ++ List.map viewExercise s.exercises
    in
        tableRow False attrs cells


viewExercise : Exercise -> Html Msg
viewExercise e =
    tableCell
        [ class "exercise-history" ]
    <|
        [ p [] [ text e.name ] ]
            ++ List.map viewSerie e.series


viewSerie : Serie -> Html Msg
viewSerie s =
    let
        str =
            Str.fromInt s.reps ++ " x " ++ Str.fromFloat s.weight ++ " kg"
    in
    p [ class "serie-history" ] [ text str ]


view : Model -> Browser.Document Msg
view model =
    let
        dateLabel =
            datePicker model.session.date

        exercises =
            viewExercisesInput model.session.exercises

        modifierButton =
            if length model.session.exercises == length exerciseList then
                addSendButton

            else
                addExerciseButton

        sessionForm =
            div [ id "session-form" ] ([ dateLabel ] ++ exercises ++ [ modifierButton ])

        errors =
            div [ id "session-form-errors" ] (List.map (p [] << List.singleton << text << second) model.errors)

        body =
            [ stylesheet
            , sessionForm
            , errors
            , viewHistory model.history
            ]
    in
    Browser.Document "Meterme" body


init : Model
init =
    Model initFakeHistory { date = "", exercises = [] } []

initFakeHistory : List Session
initFakeHistory =
    [
      { date = "date1"
      , exercises =
            [ initExercise "fake1.1"
            , initExercise "fake1.2"
            , initExercise "fake1.3"
            , initExercise "fake1.4"
            ]
      }
    , { date = "date2"
      , exercises =
            [ initExercise "fake2.1"
            , initExercise "fake2.2"
            , initExercise "fake2.3"
            , initExercise "fake2.4"
            ]
      }
    ]


main : Program () Model Msg
main =
    Browser.document
        { init = \flags -> ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
