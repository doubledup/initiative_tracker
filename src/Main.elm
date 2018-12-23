module Main exposing (main)

import Browser
import Html exposing (..)


main : Platform.Program () Model Msg
main =
    Browser.sandbox { init = init, view = view, update = update }



-- MODEL


type alias Model =
    { name : String
    }


init : Model
init =
    { name = "world"
    }



-- UPDATE


type Msg
    = None


update : Msg -> Model -> Model
update msg model =
    case msg of
        None ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Initiative tracker" ]
        ]
