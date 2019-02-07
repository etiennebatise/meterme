module HelloWorld exposing (Model, Msg(..), Series, init, main, update, view)

import Browser
import Html exposing (Attribute, Html, button, div, h1, input, option, select, text, br)
import Html.Attributes exposing (selected, value)
import Html.Events exposing (onClick, onInput)
import List exposing (append, concat, indexedMap, length, map, range, intersperse)
import Maybe


type Msg
    = UpdateExercice Int String
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
        AddExercise -> append model [ emptyExercise "squat" ]

        UpdateExercice index name ->
            indexedMap
                (\i e ->
                    if i == index then
                        { e | name = name }

                    else
                        e
                )
                model

        CreateSession ->
            [ Exercise "squat" [] ]


viewExercise : List (Attribute msg) -> Html msg
viewExercise attrs =
    select attrs
        [ option [ value "squat" ] [ text "Squat" ]
        , option [ value "pullup" ] [ text "Pull-Up" ]
        , option [ value "benchpress" ] [ text "Bench-Press" ]
        , option [ value "overheadpress" ] [ text "Overhead-Press" ]
        ]


view : Model -> Html Msg
view model =
    case model of
        [] ->
            div [] [ button [ onClick CreateSession ] [ text "Create" ] ]

        l ->
            let
                created =
                    indexedMap (\i e -> viewExercise [ value e.name, onInput (UpdateExercice i) ]) l

                new =
                     button [onClick AddExercise] [ text "Add exercice"]
            in
            div [] <| intersperse (br [] []) <| append created [ new ]



-- The hole selectWeight, Reps... is wrong. You need to somehow pass in the model
-- This example shows it a bit
-- [ input
--   [ value newComment.title
--   , onInput (\v -> toUpdateComment { newComment | title = v })
--   ]
--   []
-- newComment is the model.
-- I think the right way here is to delete all of the view logic and rewrite it with the "onclick" thing in mind


init : Model
init =
    []


main =
    Browser.sandbox { init = init, update = update, view = view }
