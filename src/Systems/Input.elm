module Systems.Input exposing (..)

import Components
import Dict
import Game exposing (Game)
import Gamepad exposing (Gamepad)
import Input
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Window


-- Create Players For Stray Inputs


addHumanPlayer : InputDevice -> Game -> Game
addHumanPlayer device game =
    game


controllerToInputDevice : Controller -> Maybe InputDevice
controllerToInputDevice controller =
    case controller of
        Human inputDevice ->
            Just inputDevice

        _ ->
            Nothing


createPlayersForStrayInputs : List Game.InputDevice -> Game -> Game
createPlayersForStrayInputs activeDevices game =
    let
        usedDevices =
            game.cPlayer
                |> Dict.values
                |> List.map .controller
                |> List.filterMap controllerToInputDevice

        hasNoPlayer device =
            not <| List.member device usedDevices

        devicesWithoutAPlayer =
            activeDevices |> List.filter hasNoPlayer
    in
        List.foldl addHumanPlayer game devicesWithoutAPlayer



-- Apply Input States to Players


type alias Args =
    { input : Input.Model
    , gamepads : List Gamepad
    , windowSizeInPixels : Window.Size
    , windowSizeInGameCoordinates : Vec2
    }


applyInputStatesToPlayers : Args -> Game -> Game
applyInputStatesToPlayers args game =
    game
