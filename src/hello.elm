module HelloWorld exposing (..)

import Browser
import Html exposing (Html, div, button, text, input)
import Html.Events exposing (onClick)


type Msg = CreateSession

type Exercise = Squat Series | PullUp Series | BenchPress Series | OverheadPress Series

type alias Series = { numberOfReps : Int
                    , weight : Int
                    }


type alias Model = List Exercise

update : Msg -> Model -> Model
update msg model = case msg of
                       CreateSession -> [Squat {numberOfReps = 0, weight = 0}]

createSessionButton = button [onClick CreateSession] [text "Create Session"]

view : Model -> Html Msg
view model = case model of
                 [] -> div [] [createSessionButton]
                 [Squat e] -> div [] [text "Squat:", text (String.fromInt e.numberOfReps), text (String.fromInt e.weight)]
                 l -> div [] [text "todo"]

init : Model
init = []

main = Browser.sandbox { init=init, update=update, view=view }
