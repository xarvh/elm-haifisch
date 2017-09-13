module App exposing (..)

import Bots
import Common exposing (Id, InputState)
import Dict exposing (Dict)
import Dict.Extra
import Game
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


type Controller
    = ControllerHuman Input.Source
    | ControllerBot Id


type alias Model =
    { bots : List Bots.Model
    , controllersAndPlayerIds : List ( Controller, Id )
    , input : Input.Model
    , game : Game.Model
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
    let
        game =
            Game.init (Random.initialSeed dateNow)

        bots =
            Bots.addBot (Bots.fullRandomBot game) []
    in
        ( { bots = bots
          , controllersAndPlayerIds = []
          , input = Input.init
          , game = game
          , windowSizeInPixels = Window.Size 1 1
          , windowSizeInGameCoordinates = vec2 1 1
          }
        , Task.perform OnWindowResizes Window.size
        )



-- mouse aim


getPlayerMouseAim : Model -> Input.Source -> Mouse.Position -> Vec2
getPlayerMouseAim model inputSource mousePositionInWindowCoordinates =
    let
        findPlayerIdThatHasController controller =
            List.Extra.find (\( c, playerId ) -> c == controller) model.controllersAndPlayerIds
                |> Maybe.map Tuple.second

        findShipOwnedByPlayerId playerId =
            Dict.Extra.find (\shipId ship -> ship.playerId == playerId) model.game.shipsById

        maybeControlledShip =
            findPlayerIdThatHasController (ControllerHuman inputSource)
                |> Maybe.andThen findShipOwnedByPlayerId
    in
        case maybeControlledShip of
            Nothing ->
                vec2 0 1

            Just ( shipId, ship ) ->
                let
                    -- Window coordinates, in pixel (Y axis points down)
                    mousePixelX =
                        toFloat mousePositionInWindowCoordinates.x

                    mousePixelY =
                        toFloat mousePositionInWindowCoordinates.y

                    pixelW =
                        toFloat model.windowSizeInPixels.width

                    pixelH =
                        toFloat model.windowSizeInPixels.height

                    -- Game coordinates (Y axis points up)
                    ( gameW, gameH ) =
                        Vec2.toTuple model.windowSizeInGameCoordinates

                    mouseGameX =
                        gameW * (mousePixelX - pixelW / 2) / pixelW

                    mouseGameY =
                        gameH * (pixelH / 2 - mousePixelY - 1) / pixelH

                    mouseGamePos =
                        vec2 mouseGameX mouseGameY
                in
                    Vec2.sub mouseGamePos ship.position |> Vec2.normalize


resolveMouseAim : Model -> Input.Source -> Input.RawInputState -> InputState
resolveMouseAim model inputSource rawInputState =
    let
        finalAim =
            case rawInputState.finalAim of
                Input.Direction direction ->
                    direction

                Input.ScreenPosition position ->
                    getPlayerMouseAim model inputSource position
    in
        { finalAim = finalAim
        , fire = rawInputState.fire
        , move = rawInputState.move
        }



-- input


manageInput : Config -> Gamepad.Blob -> Model -> ( Model, Dict Id InputState )
manageInput config blob model =
    let
        inputSourcesAndRawStates =
            Input.sourcesAndStates
                config.maybeInputConfig
                (Gamepad.getGamepads config.gamepadDatabase blob)
                model.input

        humanControllersAndStates =
            inputSourcesAndRawStates
                |> List.map (\( source, raw ) -> ( ControllerHuman source, resolveMouseAim model source raw ))

        botsAndStates =
            model.bots |> List.map (Bots.think model.game)

        botControllersAndStates =
            botsAndStates |> List.map (Tuple.mapFirst (.id >> ControllerBot))

        controllersAndStates =
            humanControllersAndStates ++ botControllersAndStates

        --
        findPlayerId ( controller, state ) =
            model.controllersAndPlayerIds
                |> List.Extra.find (\( c, id ) -> c == controller)
                |> Maybe.map (\( c, id ) -> ( id, state ))

        stateByPlayerId =
            controllersAndStates
                |> List.filterMap findPlayerId
                |> Dict.fromList

        --
        controllersWithAPlayer =
            model.controllersAndPlayerIds
                |> List.map Tuple.first

        controllersWithoutAPlayer =
            controllersAndStates
                |> List.map Tuple.first
                |> List.filter (\c -> not <| List.member c controllersWithAPlayer)

        addPlayer : Controller -> ( Game.Model, List ( Controller, Id ) ) -> ( Game.Model, List ( Controller, Id ) )
        addPlayer controller ( oldGame, cAndIds ) =
            let
                ( newGame, player ) =
                    Game.addPlayer oldGame
            in
                ( newGame, ( controller, player.id ) :: cAndIds )

        ( game, controllersAndPlayerIds ) =
            controllersWithoutAPlayer
                |> List.foldl addPlayer ( model.game, model.controllersAndPlayerIds )
    in
        ( { model
            | game = game
            , controllersAndPlayerIds = controllersAndPlayerIds
            , bots = botsAndStates |> List.map Tuple.first
          }
        , stateByPlayerId
        )



-- update


noCmd model =
    ( model, Cmd.none )


resizeWindow : Window.Size -> Model -> Model
resizeWindow sizeInPixels model =
    let
        internalCoordinatesHeight =
            Game.worldRadius * 2.1

        internalCoordinatesWidth =
            toFloat sizeInPixels.width * internalCoordinatesHeight / toFloat sizeInPixels.height
    in
        { model
            | windowSizeInPixels = sizeInPixels
            , windowSizeInGameCoordinates = vec2 internalCoordinatesWidth internalCoordinatesHeight
        }


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


updateAnimationFrame : Config -> Time -> Gamepad.Blob -> Model -> ( Model, Cmd Msg )
updateAnimationFrame config dt blob oldModel =
    let
        ( model, inputStatesByPlayerId ) =
            manageInput config blob oldModel

        ( game, events ) =
            Game.tick inputStatesByPlayerId dt model.game

        cmds =
            List.map eventToCmd events
    in
        ( { model | game = game }, Cmd.batch cmds )


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
        , View.Scoreboard.scoreboard model.game.players model.game.shipsById
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes OnWindowResizes
        , Input.subscriptions model.input |> Sub.map OnInputMsg
        , GamepadPort.gamepad OnAnimationFrame
        ]
