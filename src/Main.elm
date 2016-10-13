module Main exposing (..)

import Gamepad exposing (Gamepad)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.App
import List.Extra
import Random
import Random.Extra
import Ship exposing (Ship)
import Time exposing (Time)
import View


-- Model


type alias Model =
    { ships : List Ship
    , seed : Random.Seed
    }



-- Game logic


updateShipWithGamepad gamepad dt ship =
    ship


explodeShip dt ship =
    ship


iterateTime : Time -> List Gamepad -> Model -> ( Model, Cmd Msg )
iterateTime dt gamepads model =


        {- there are three cases than need be covered:
            * ships associated to a gamepad
            * ships without gamepad
            * gamepads without ships
        -}

        folder ship (shipsAndGamepads, shipsWithoutGamepad, remainingGamepads) =
            case List.Extra.find (\gp -> gp.index == ship.controllerId) remainingGamepads of
                Just gamepad ->
                    ( ( ship, gamepad ) :: shipsAndGamepads, shipsWithoutGamepad, List.Extra.remove gamepad remainingGamepads )

                Nothing ->
                    ( shipsAndGamepads, ship :: shipsWithoutGamepad, remainingGamepads )

       (shipsAndGamepads, shipsWithoutGamepad, gamepadsWithoutShip) =
           List.foldl folder ([], [], gamepads) model.ships




        -- manages ships, with or without gamepads
        mapShip gamepads ship =
            case List.Extra.find (\gp -> gp.index == ship.controllerId) gamepads of
                Nothing ->
                    explodeShip dt ship

                Just gamepad ->
                    updateShipWithGamepad gamepad dt ship

        shipsAlreadyPresent =
            List.map (mapShip gamepads) model.ships

        -- manages gamepads without ship
        foldGamepad ships gamepad ( additionalShips, oldSeed ) =
            case List.Extra.find (\ship -> ship.controllerId == gamepad.index) ships of
                Just ship ->
                    ( additionalShips, oldSeed )

                Nothing ->
                    let
                        ( newShip, newSeed ) =
                            newShip gamepad.index oldSeed
                    in
                        ( newShip :: additionalShips, newSeed )

        ( shipsToBeAdded, newSeed ) =
            List.foldl (foldGamepad model.ships) ( [], model.seed ) gamepads
    in
        Model (shipsAlreadyPresent ++ shipsToBeAdded) newSeed ! []



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
            iterateTime dt gamepads model


init : Int -> ( Model, Cmd Msg )
init dateNow =
    Model [] (Random.initialSeed dateNow) ! []


view : Model -> Html Msg
view model =
    H.div
        [ HA.class "ui"
        ]
        [ View.background
        , Html.App.map (always Noop) (View.starSystemBox model.ships)
        ]


main =
    Html.App.programWithFlags
        { init = init
        , update = update
        , subscriptions = \model -> Gamepad.animationFrameAndGamepads AnimationFrameAndGamepads
        , view = view
        }
