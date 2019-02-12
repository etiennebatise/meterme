module HelloWorld exposing (Model, Msg(..), Series, init, main, update, view)

import Browser
import Html exposing (Attribute, Html, br, button, div, h1, input, option, select, text)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onClick, onInput)
import List exposing (append, concat, indexedMap, intersperse, length, map, range)
import Maybe


type Msg
    = UpdateExercice Int String
    | UpdateSerie Int String
    | CreateSession
    | AddExercise


type alias Series =
    { numberOfReps : Int
    , weight : Int
    }


type alias Training =
    List Series


type alias Exercise =
    { name : String
    , training : Training
    }



-- type Session = (List Training, Date, Comment, other stuff)


type alias Session =
    List Exercise


type alias Model =
    Session


emptyExercise : String -> Exercise
emptyExercise name =
    Exercise name []


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddExercise ->
            append model [ emptyExercise "squat" ]

        UpdateExercice index name ->
            indexedMap
                (\i e ->
                    if i == index then
                        { e | name = name }

                    else
                        e
                )
                model

        UpdateSerie index nbReps ->
            indexedMap
                (\i e ->
                    if i == index then
                        { e | training = [ Series (Maybe.withDefault 0 <| String.toInt nbReps) 0]}

                    else
                        e
                )
                model


        CreateSession ->
            [ Exercise "squat" [] ]


optionRange : Int -> Int -> List (Html msg)
optionRange from to = let l = range from to
                      in map (\i -> option [value <| String.fromInt i] [text <| String.fromInt i]) l

viewExercise : Int -> List (Attribute Msg) -> Html Msg
viewExercise index attrs =
    div []
    [ select attrs
          [ option [ value "squat" ] [ text "Squat" ]
          , option [ value "pullup" ] [ text "Pull-Up" ]
          , option [ value "benchpress" ] [ text "Bench-Press" ]
          , option [ value "overheadpress" ] [ text "Overhead-Press" ]
          ]
    , select [onInput (UpdateSerie index)] (optionRange 1 12)
    ]


view : Model -> Html Msg
view model =
    case model of
        [] ->
            div [] [ button [ onClick CreateSession ] [ text "Create" ] ]

        l ->
            let
                created =
                    indexedMap (\i e -> viewExercise i [ value e.name, onInput (UpdateExercice i) ]) l

                new =
                    button [ onClick AddExercise ] [ text "Add exercice" ]
            in
            div [] <| intersperse (br [] []) <| append created [ new ]


init : Model
init =
    []


main =
    Browser.sandbox { init = init, update = update, view = view }
