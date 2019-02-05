module HelloWorld exposing (Exercise(..), Model, Msg(..), Series, createSessionButton, init, main, selectExercise, update, view)

import Browser
import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onClick)
import List exposing (append, concat, map, range)


type Msg
    = CreateSession
    | AddSeries
    | ValidateForm


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

        AddSeries ->
            append model [ Squat { numberOfReps = 0, weight = 0 } ]

        ValidateForm ->
            []


createSessionButton =
    button [ onClick CreateSession ] [ text "Create Session" ]


selectExercise : Html msg
selectExercise =
    select []
        [ option [ value "squat" ] [ text "Squat" ]
        , option [ value "pullup" ] [ text "Pull-Up" ]
        , option [ value "benchpress" ] [ text "Bench-Press" ]
        , option [ value "overheadpress" ] [ text "Overhead-Press" ]
        ]


selectNumberOfReps : Int -> Html msg
selectNumberOfReps default =
    select []
        (map
            (\i ->
                let
                    s =
                        String.fromInt i
                in
                option [ value s, selected (i == default) ] [ text s ]
            )
            (range 1 12)
        )


selectWeight : Int -> Html msg
selectWeight default =
    select []
        (map
            (\i ->
                let
                    s =
                        String.fromInt i
                in
                option [ value s, selected (i == default) ] [ text s ]
            )
            (range -30 100)
        )


addSeries : Html Msg
addSeries =
    button [ onClick AddSeries ] [ text "+" ]


row : Int -> Int -> List (Html Msg)
row reps weight =
    [ selectExercise, selectNumberOfReps reps, selectWeight weight, addSeries ]


validateButton : Html Msg
validateButton =
    button [ onClick ValidateForm ] [ text "Validate" ]


view : Model -> Html Msg
view model =
    case model of
        [] ->
            div [] [ createSessionButton ]

        [ Squat e ] ->
            div [] (row e.numberOfReps e.weight)

        l ->
            div [] <|
                append
                    (map
                        (\m ->
                            case m of
                                Squat n ->
                                    div [] (row n.numberOfReps n.weight)

                                z ->
                                    text "Todo"
                        )
                        l
                    )
                    [ validateButton ]


init : Model
init =
    []


main =
    Browser.sandbox { init = init, update = update, view = view }
