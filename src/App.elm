module App exposing (..)

-- import Bots

import Common exposing (Id, InputState)
import Dict exposing (Dict)
import Dict.Extra
import Game exposing (Game)
import Gamepad
import GamepadPort
import Html exposing (..)
import Input
import List.Extra
import LocalStoragePort
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Mouse
import MousePort
import Random
import SoundPort
import Systems.Input
import Systems.Movement
import Systems.Ship
import Systems.SpawnShips
import Task
import Time exposing (Time)
import View
import View.Background
import View.Scoreboard
import Window


-- types


type alias Config =
    { maybeInputConfig : Maybe Input.Config
    , gamepadDatabase : Gamepad.Database
    }


type alias Model =
    { input : Input.Model
    , game : Game
    , windowSizeInPixels : Window.Size
    , windowSizeInGameCoordinates : Vec2
    }


type Msg
    = OnAnimationFrame ( Time, Gamepad.Blob ) -- This msg is called directly by Config
    | OnInputMsg Input.Msg
    | OnWindowResizes Window.Size



-- init


init : Int -> ( Model, Cmd Msg )
init dateNow =
    ( { input = Input.init
      , game = Game.init (Random.initialSeed dateNow)
      , windowSizeInPixels = Window.Size 1 1
      , windowSizeInGameCoordinates = vec2 1 1
      }
    , Task.perform OnWindowResizes Window.size
    )



-- update


noCmd model =
    ( model, Cmd.none )


resizeWindow : Window.Size -> Model -> Model
resizeWindow sizeInPixels model =
    let
        internalCoordinatesHeight =
            Common.worldRadius * 2.1

        internalCoordinatesWidth =
            toFloat sizeInPixels.width * internalCoordinatesHeight / toFloat sizeInPixels.height
    in
        { model
            | windowSizeInPixels = sizeInPixels
            , windowSizeInGameCoordinates = vec2 internalCoordinatesWidth internalCoordinatesHeight
        }


windowToGameCoordinates : Model -> { x : Int, y : Int } -> Vec2
windowToGameCoordinates model positionInPixels =
    let
        -- Window coordinates, in pixel (Y axis points down)
        pixelX =
            toFloat positionInPixels.x

        pixelY =
            toFloat positionInPixels.y

        pixelW =
            toFloat model.windowSizeInPixels.width

        pixelH =
            toFloat model.windowSizeInPixels.height

        -- Game coordinates (Y axis points up)
        ( gameW, gameH ) =
            Vec2.toTuple model.windowSizeInGameCoordinates

        gameX =
            gameW * (pixelX - pixelW / 2) / pixelW

        gameY =
            gameH * (pixelH / 2 - pixelY - 1) / pixelH
    in
        vec2 gameX gameY



{-
   eventToCmd : Common.Event -> Cmd msg
   eventToCmd event =
       case event of
           Common.ShipExplodes _ ->
               SoundPort.playSound "explosion"

           Common.ShipFires _ ->
               SoundPort.playSound "fire"

           Common.ShipActivates _ ->
               SoundPort.playSound "spawnEnd"

           Common.ShipAppears _ ->
               Cmd.none

           Common.ShipDamagesShip _ _ ->
               Cmd.none
-}


updateAnimationFrame : Config -> Time -> Gamepad.Blob -> Model -> ( Model, Cmd Msg )
updateAnimationFrame config dt blob model =
    let
        gamepads =
            Gamepad.getGamepads config.gamepadDatabase blob

        activeInputDevices =
            Input.activeInputDevices config.maybeInputConfig gamepads

        inputSystemArgs =
            { input = model.input
            , gamepads = gamepads
            , useKeyboardAndMouse = Input.config config.maybeInputConfig gamepads == Input.OnePlayerUsesKeyboardAndMouse
            , windowToGameCoordinates = windowToGameCoordinates model
            }

        game =
            model.game
                |> Systems.Input.createPlayersForStrayInputs activeInputDevices
                |> Systems.Input.applyInputStatesToPlayers inputSystemArgs
                |> Systems.SpawnShips.spawnShipsForPlayersWithoutAShip
                |> Systems.Ship.shipsSystems dt
                |> Systems.Movement.movement dt
    in
        ( { model | game = game }, Cmd.none )


update : Bool -> Config -> Msg -> Model -> ( Model, Cmd Msg )
update isPaused config msg model =
    case msg of
        OnAnimationFrame ( dt, blob ) ->
            if isPaused then
                noCmd model
            else
                updateAnimationFrame config dt blob model

        OnInputMsg msg ->
            { model | input = Input.update msg model.input } |> noCmd

        OnWindowResizes size ->
            model
                |> resizeWindow size
                |> noCmd



-- view


view : Model -> Html Msg
view model =
    div
        []
        [ View.Background.view
        , View.game model.windowSizeInGameCoordinates model.game
        , View.Scoreboard.scoreboard model.game
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes OnWindowResizes
        , Input.subscriptions model.input |> Sub.map OnInputMsg
        , GamepadPort.gamepad OnAnimationFrame
        ]
