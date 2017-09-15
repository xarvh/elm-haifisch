module Input exposing (..)

import Dict exposing (Dict)
import Game
import Gamepad exposing (Gamepad)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Mouse
import MousePort
import Keyboard.Extra exposing (Key)
import Time exposing (Time)


-- types


type Config
    = AllPlayersUseGamepads
    | OnePlayerUsesKeyboardAndMouse


type Msg
    = OnKeyboardMsg Keyboard.Extra.Msg
    | OnMouseButton ( Int, Bool )
    | OnMouseMove Mouse.Position


type alias Model =
    { mousePosition : Mouse.Position
    , mouseButtonLeft : Bool
    , mouseButtonMiddle : Bool
    , mouseButtonRight : Bool
    , pressedKeys : List Key
    }



-- init


v0 : Vec2
v0 =
    vec2 0 0


init : Model
init =
    { mousePosition = Mouse.Position 0 0
    , mouseButtonLeft = False
    , mouseButtonMiddle = False
    , mouseButtonRight = False
    , pressedKeys = []
    }



-- State


config : Maybe Config -> List Gamepad -> Config
config maybeConfig gamepads =
    case maybeConfig of
        Just config ->
            config

        Nothing ->
            if List.length gamepads > 0 then
                AllPlayersUseGamepads
            else
                OnePlayerUsesKeyboardAndMouse


activeInputDevices : Maybe Config -> List Gamepad -> List Game.InputDevice
activeInputDevices maybeConfig gamepads =
    let
        keyboardAndMouseDevices =
            case config maybeConfig gamepads of
                AllPlayersUseGamepads ->
                    []

                OnePlayerUsesKeyboardAndMouse ->
                    [ Game.InputDeviceKeyboardAndMouse ]

        gamepadDevices =
            gamepads
                |> List.map (Gamepad.getIndex >> Game.InputDeviceGamepad)
    in
        keyboardAndMouseDevices ++ gamepadDevices


keyboardAndMouseInputState : Model -> (Mouse.Position -> Vec2) -> Game.InputState
keyboardAndMouseInputState model windowToGameCoordinates =
    let
        mousePositionInGameCoordinates =
            windowToGameCoordinates model.mousePosition

        { x, y } =
            Keyboard.Extra.wasd model.pressedKeys

        move =
            vec2 (toFloat x) (toFloat y)
    in
        { finalAim = Game.AimAbsolute mousePositionInGameCoordinates
        , fire = model.mouseButtonLeft
        , move = move
        }


gamepadToInputState : Gamepad -> Game.InputState
gamepadToInputState gamepad =
    let
        aimDirection =
            vec2 (Gamepad.rightX gamepad) (Gamepad.rightY gamepad)

        fire =
            Gamepad.aIsPressed gamepad

        move =
            vec2 (Gamepad.leftX gamepad) (Gamepad.leftY gamepad)
    in
        { finalAim = Game.AimRelative aimDirection
        , fire = fire
        , move = move
        }



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnKeyboardMsg keyboardMsg ->
            { model | pressedKeys = Keyboard.Extra.update keyboardMsg model.pressedKeys }

        OnMouseButton ( button, toggle ) ->
            case button of
                0 ->
                    { model | mouseButtonLeft = toggle }

                1 ->
                    { model | mouseButtonMiddle = toggle }

                _ ->
                    { model | mouseButtonRight = toggle }

        OnMouseMove position ->
            { model | mousePosition = position }



-- subscriptions


subscriptions model =
    -- TODO disable the subscriptions if key&mouse are not used
    Sub.batch
        [ Sub.map OnKeyboardMsg Keyboard.Extra.subscriptions
        , Mouse.moves OnMouseMove
        , MousePort.button OnMouseButton
        ]
