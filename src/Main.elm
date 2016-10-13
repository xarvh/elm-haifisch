module Main exposing (..)

import Dict
import Gamepad exposing (Gamepad)
import Game exposing (vector)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.App
import List.Extra
import Random
import Task
import Window
import Time exposing (Time)
import View


-- Model


type alias Model =
    { game : Game.Model
    , hasGamepads : Bool
    , windowSizeInPixels : Window.Size
    , windowSizeInGameCoordinates : Game.Vector
    }



-- Gamepad guessing


guessGamepad gamepad =
    case gamepad.axes of
        -- Some Xbox 360 in Chrom
        xLeftStick :: yLeftStick :: xRightStick :: yRightStick :: [] ->
            ( vector xLeftStick yLeftStick, vector xRightStick yRightStick, False )

        xLeftStick :: yLeftStick :: leftTrigger :: xRightStick :: yRightStick :: yRightTrigger :: [] ->
            ( vector xLeftStick yLeftStick, vector xRightStick yRightStick, False )

        -- Xbox 360 in Firefox
        xLeftStick :: yLeftStick :: leftTrigger :: xRightStick :: yRightStick :: yRightTrigger :: xPad :: yPad :: [] ->
            ( vector xLeftStick yLeftStick, vector xRightStick yRightStick, False )

        _ ->
            ( vector 0 0, vector 0 0, False )



-- Game logic


gamepadToCommand gamepad =
    guessGamepad gamepad


gamepadShip ( ship, gamepad ) =
    Game.update <| Game.ControlShip ship <| guessGamepad gamepad


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
        { model | game = newGame, hasGamepads = gamepads /= [] } ! []


resizeWindow : Window.Size -> Model -> Model
resizeWindow sizeInPixels model =
    let
        internalCoordinatesHeight =
            Game.starSystemOuterRadius * 2.1

        internalCoordinatesWidth =
            toFloat sizeInPixels.width * internalCoordinatesHeight / toFloat sizeInPixels.height
    in
        { model
            | windowSizeInPixels = sizeInPixels
            , windowSizeInGameCoordinates = Game.vector internalCoordinatesWidth internalCoordinatesHeight
        }



-- Boilerplate stuff


type Msg
    = Noop
    | WindowResizes Window.Size
    | AnimationFrameAndGamepads ( Time, List Gamepad )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            model ! []

        WindowResizes windowSize ->
            resizeWindow windowSize model ! []

        AnimationFrameAndGamepads ( dt, gamepads ) ->
            -- Work around a bug in Gamepad https://github.com/xarvh/elm-gamepad/issues/2
            if dt < 0 then
                model ! []
            else
                animationFrame dt gamepads model


init : Int -> ( Model, Cmd Msg )
init dateNow =
    { game = (Game.init dateNow)
    , hasGamepads = False
    , windowSizeInPixels = { width = 800, height = 600 }
    , windowSizeInGameCoordinates = (Game.vector 4 3)
    }
        ! [ Task.perform identity WindowResizes Window.size ]


view : Model -> Html Msg
view model =
    H.div
        [ HA.class "ui"
        ]
        [ View.background
        , Html.App.map (always Noop) (View.game model.windowSizeInGameCoordinates model.game)
        , View.splash model.hasGamepads
        ]


main =
    Html.App.programWithFlags
        { init = init
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ Gamepad.animationFrameAndGamepads AnimationFrameAndGamepads
                    , Window.resizes WindowResizes
                    ]
        , view = view
        }
