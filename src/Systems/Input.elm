module Systems.Input exposing (..)

import Components exposing (EntityId)
import Dict
import Game exposing (Game, InputDevice)
import Gamepad exposing (Gamepad)
import Input
import List.Extra
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Window


-- Create Players For Stray Inputs


addHumanPlayer : InputDevice -> Game -> Game
addHumanPlayer device game =
    Game.addPlayer (Game.ControllerHuman device) game |> Tuple.first


controllerToInputDevice : Game.Controller -> Maybe InputDevice
controllerToInputDevice controller =
    case controller of
        Game.ControllerHuman inputDevice ->
            Just inputDevice

        _ ->
            Nothing


createPlayersForStrayInputs : List InputDevice -> Game -> Game
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
    , useKeyboardAndMouse : Bool
    , windowToGameCoordinates : { x : Int, y : Int } -> Vec2
    }


deviceInputState : Args -> InputDevice -> Maybe Game.InputState
deviceInputState args device =
    case device of
        Game.InputDeviceGamepad index ->
            args.gamepads
                |> List.Extra.find (\g -> Gamepad.getIndex g == index)
                |> Maybe.map Input.gamepadToInputState

        Game.InputDeviceKeyboardAndMouse ->
            if not args.useKeyboardAndMouse then
                Nothing
            else
                Just (Input.keyboardAndMouseInputState args.input args.windowToGameCoordinates)


applyInputState : Args -> EntityId -> Game.PlayerComponent -> Game.PlayerComponent
applyInputState args playerId player =
    case player.controller of
        Game.ControllerBot _ ->
            player

        Game.ControllerHuman device ->
            { player | inputState = deviceInputState args device }


applyInputStatesToPlayers : Args -> Game -> Game
applyInputStatesToPlayers args game =
    { game | cPlayer = Dict.map (applyInputState args) game.cPlayer }
