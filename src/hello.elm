module HelloWorld exposing (Exercise(..), Model, Msg(..), Series, createSessionButton, init, main, selectExercise, update, view)

import List exposing (map, range)
import Browser
import Html exposing (Html, button, div, input, select, text, option)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick)


type Msg
    = CreateSession


type Exercise
    = Squat Series
    | PullUp Series
    | BenchPress Series
    | OverheadPress Series


type alias Series =
    { numberOfReps : Int
    , weight : Int
    }


type alias Model =
    List Exercise


update : Msg -> Model -> Model
update msg model =
    case msg of
        CreateSession ->
            [ Squat { numberOfReps = 0, weight = 0 } ]


createSessionButton =
    button [ onClick CreateSession ] [ text "Create Session" ]


selectExercise : Html msg
selectExercise =
    select []
        [ option [value "squat"] [ text "Squat" ]
        , option [value "pullup"] [ text "Pull-Up" ]
        , option [value "benchpress"] [ text "Bench-Press" ]
        , option [value "overheadpress"] [ text "Overhead-Press" ]
        ]

selectNumberOfReps : Html msg
selectNumberOfReps =
    select [] (map (\i -> let s = String.fromInt i in option [value s] [text s]) (range 1 12))

selectWeight : Html msg
selectWeight = 
    select [] (map (\i -> let s = String.fromInt i in option [value s] [text s]) (range -30 100))

view : Model -> Html Msg
view model =
    case model of
        [] ->
            div [] [ createSessionButton ]

        [ Squat e ] ->
            div [] [ selectExercise, selectNumberOfReps, selectWeight ]

        l ->
            div [] [ text "todo" ]


init : Model
init =
    []


main =
    Browser.sandbox { init = init, update = update, view = view }
