module HelloWorld exposing (..)

import Browser
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)


type Msg = Increment | Decrement

type alias Model = Int

-- update :: Msg -> Model -> _
update msg model =
    case msg of
        Increment ->
            model + 1
        Decrement ->
            model - 1

view model = div []
             [ div [] [text (String.fromInt  model)]
             , button [onClick Decrement] [text "-"]
             , button [onClick Increment] [text "+"]
             ]

init = 0

main = Browser.sandbox { init=init, update=update, view=view }
