module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { characters : Array.Array Character
    , nextCharacter : Character
    }


type alias Character =
    { name : String
    , initiativeMod : Int
    , initiative : Int
    }


emptyCharacter =
    { name = ""
    , initiativeMod = 0
    , initiative = 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { characters = Array.empty
      , nextCharacter = emptyCharacter
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateNextCharacter String Int
    | AddCharacter
    | RollInitiative
    | SetInitiative Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateNextCharacter name initiativeMod ->
            ( { model
                | nextCharacter =
                    { name = name
                    , initiativeMod = initiativeMod
                    , initiative = 0
                    }
              }
            , Cmd.none
            )

        AddCharacter ->
            ( { model
                | characters = Array.push model.nextCharacter model.characters
                , nextCharacter = emptyCharacter
              }
            , rollInitiative (Array.length model.characters) model.nextCharacter.initiativeMod
            )

        RollInitiative ->
            ( model
            , Cmd.batch
                << Array.toList
              <|
                Array.indexedMap rollInitiative
                    (Array.map .initiativeMod model.characters)
            )

        SetInitiative i v ->
            let
                maybeCharacter =
                    Array.get i model.characters
            in
            case maybeCharacter of
                Just character ->
                    ( { model
                        | characters =
                            Array.set
                                i
                                { character | initiative = v }
                                model.characters
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


rollInitiative i m =
    Random.generate (\v -> SetInitiative i (v + m)) rolld20


rolld20 =
    Random.int 1 20



-- SUBSCRIPTIONS


subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Initiative tracker" ]
        , div []
            [ text "Name "
            , input
                [ onInput
                    (\name -> UpdateNextCharacter name model.nextCharacter.initiativeMod)
                ]
                []
            ]
        , div []
            [ text "Initiative "
            , input
                [ onInput
                    (\initiative ->
                        case String.toInt initiative of
                            Just i ->
                                UpdateNextCharacter model.nextCharacter.name i

                            Nothing ->
                                UpdateNextCharacter model.nextCharacter.name 0
                    )
                ]
                []
            ]
        , button [ onClick AddCharacter ] [ text "Add" ]
        , div []
            [ text "Next character: "
            , p []
                [ div [] [ text <| "Name: " ++ model.nextCharacter.name ++ " " ]
                , div [] [ text <| "Initiative: " ++ String.fromInt model.nextCharacter.initiativeMod ]
                ]
            ]
        , h2 [] [ text "Characters" ]
        , ul [] <|
            List.map (\c -> li [] [ text <| showCharacterInitiative c ])
                (List.sortBy (\c -> -c.initiative) <| Array.toList model.characters)
        , button [ onClick RollInitiative ] [ text "Roll Initiative!" ]
        ]


showCharacterInitiative : Character -> String
showCharacterInitiative c =
    c.name ++ "|" ++ String.fromInt c.initiative ++ "|" ++ String.fromInt c.initiativeMod
