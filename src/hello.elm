module HelloWorld exposing (..)

import Browser
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)


type Msg = IncrementSquat | DecrementSquat
         | IncrementPullUp | DecrementPullUp
         | IncrementBenchPress | DecrementBenchPress
         | IncrementOverheadPress | DecrementOverheadPress

type alias Model = {
        squat : Int
      , pullUp : Int
      , benchPress : Int
      , overheadPress : Int
    }

update msg model =
    case msg of
        IncrementSquat ->
            {model | squat = model.squat + 1}
        DecrementSquat ->
            {model | squat = model.squat - 1}
        IncrementPullUp ->
            {model | pullUp = model.pullUp  + 1}
        DecrementPullUp ->
            {model | pullUp = model.pullUp  - 1}
        IncrementBenchPress ->
            {model | benchPress = model.benchPress  + 1}
        DecrementBenchPress ->
            {model | benchPress = model.benchPress  - 1}
        IncrementOverheadPress ->
            {model | overheadPress = model.overheadPress  + 1}
        DecrementOverheadPress ->
            {model | overheadPress = model.overheadPress  - 1}

opTemplate opLabel msg = button [onClick msg] [text opLabel]

incTemplate = opTemplate "+"

decTemplate = opTemplate "-"

viewTemplate name value msgA msgB = div []
                                    [ div [] [text (String.fromInt value)]
                                    , decTemplate msgA
                                    , incTemplate msgB
                                    ]

view model = div []
             [ viewTemplate "Squats" model.squat DecrementSquat IncrementSquat
             , viewTemplate "PullUps" model.pullUp DecrementPullUp IncrementPullUp
             , viewTemplate "BenchPress" model.benchPress DecrementBenchPress IncrementBenchPress
             , viewTemplate "OverheadPresss" model.overheadPress DecrementOverheadPress IncrementOverheadPress
             ]

init = { squat = 0, pullUp = 0, benchPress = 0, overheadPress = 0 }

main = Browser.sandbox { init=init, update=update, view=view }
