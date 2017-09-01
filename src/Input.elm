module Input exposing (..)

import Gamepad exposing (Gamepad)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Mouse
import MousePort
import Keyboard.Extra exposing (Key)
import Time exposing (Time)


-- types


type FinalAim
    = Direction Vec2
    | ScreenPosition Mouse.Position


type alias InputState =
    { finalAim : FinalAim
    , fire : Bool
    , move : Vec2
    }


type Config
    = AllPlayersUseGamepads
    | Player1UsesKeyboardAndMouse


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



-- input to player input state


keyboardAndMouseToInputState : Model -> InputState
keyboardAndMouseToInputState model =
    let
        pixelsForAFullTurn =
            2000

        turningRatio =
            turns 1 / pixelsForAFullTurn

        { x, y } =
            Keyboard.Extra.wasd model.pressedKeys

        move =
            vec2 (toFloat x) (toFloat y)
    in
        { finalAim = ScreenPosition model.mousePosition
        , fire = model.mouseButtonLeft
        , move = move
        }


gamepadToInputState : Gamepad -> InputState
gamepadToInputState gamepad =
    let
        aimDirection =
            vec2 (Gamepad.rightX gamepad) -(Gamepad.rightY gamepad) |> Vec2.normalize

        fire =
            Gamepad.aIsPressed gamepad

        move =
            vec2 (Gamepad.leftX gamepad) (Gamepad.leftY gamepad)
    in
        { finalAim = Direction aimDirection
        , fire = fire
        , move = move
        }



-- input assignment


inputStates : Maybe Config -> List Gamepad -> Model -> List InputState
inputStates maybeConfig gamepads model =
    let
        config =
            case maybeConfig of
                Just config ->
                    config

                Nothing ->
                    if List.length gamepads > 0 then
                        AllPlayersUseGamepads
                    else
                        Player1UsesKeyboardAndMouse

        keyboardInputs =
            case config of
                AllPlayersUseGamepads ->
                    []

                Player1UsesKeyboardAndMouse ->
                    [ keyboardAndMouseToInputState model ]

        gamepadInputs =
            gamepads
                |> List.sortBy Gamepad.getIndex
                |> List.map gamepadToInputState
    in
        keyboardInputs ++ gamepadInputs



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
