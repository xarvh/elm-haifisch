module Main exposing (..)

import Dict
import Gamepad exposing (Gamepad)
import Game exposing (vector)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.App
import List.Extra
import Random


-- import Random.Extra

import Time exposing (Time)
import View


-- Model


type alias Model =
    { game : Game.Model
    }



-- Game logic


gamepadToCommand gamepad =
    -- TODO
    ( vector 0 0, vector 0 0, False )


gamepadShip ( ship, gamepad ) =
    Game.update <| Game.ControlShip ship <| gamepadToCommand gamepad


removeShip ship =
    Game.update (Game.RemoveShip ship)


addShip gamepad =
    Game.update (Game.AddShip gamepad.index)




animationFrame : Time -> List Gamepad -> Model -> ( Model, Cmd Msg )
animationFrame dt gamepads model =
    let
        {- there are three cases than need be covered:
           * ships associated to a gamepad
           * ships without gamepad
           * gamepads without ships
        -}
        folder ship ( shipsAndGamepads, shipsWithoutGamepad, remainingGamepads ) =
            case List.Extra.find (\gp -> gp.index == ship.controllerId) remainingGamepads of
                Just gamepad ->
                    ( ( ship, gamepad ) :: shipsAndGamepads, shipsWithoutGamepad, List.Extra.remove gamepad remainingGamepads )

                Nothing ->
                    ( shipsAndGamepads, ship :: shipsWithoutGamepad, remainingGamepads )

        ( shipsAndGamepads, shipsWithoutGamepad, gamepadsWithoutShip ) =
            List.foldl folder ( [], [], gamepads ) (Dict.values model.game.shipsById)

        apply : (a -> b -> b) -> List a -> b -> b
        apply f list oldB =
            List.foldl f oldB list

        newGame =
            model.game
                |> apply gamepadShip shipsAndGamepads
                |> apply removeShip shipsWithoutGamepad
                |> apply addShip gamepadsWithoutShip
                |> Game.update (Game.Tick dt)
    in
        { model | game = newGame } ! []



-- Boilerplate stuff


type Msg
    = Noop
    | AnimationFrameAndGamepads ( Time, List Gamepad )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        AnimationFrameAndGamepads ( dt, gamepads ) ->
            animationFrame dt gamepads model


init : Int -> ( Model, Cmd Msg )
init dateNow =
    Model (Game.init <| Random.initialSeed dateNow) ! []


view : Model -> Html Msg
view model =
    H.div
        [ HA.class "ui"
        ]
        [ View.background
        , Html.App.map (always Noop) (View.game model.game)
        ]


main =
    Html.App.programWithFlags
        { init = init
        , update = update
        , subscriptions = \model -> Gamepad.animationFrameAndGamepads AnimationFrameAndGamepads
        , view = view
        }
