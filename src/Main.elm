module Main exposing (..)

import Array exposing (Array)
import Common exposing (..)
import Dict exposing (Dict)
import Gamepad exposing (Gamepad)
import Gamepad.Remap
import GamepadPort
import LocalStoragePort
import Game exposing ((|>>))
import Html as H exposing (Html)
import Html.Attributes as HA
import List.Extra
import Ports
import Random
import Random.Array
import Task
import Window
import Time exposing (Time)
import View


-- types


type Msg
    = Noop
    | WindowResizes Window.Size
    | AnimationFrameAndGamepads ( Time, Gamepad.Blob )


type Status
    = NoGamepads
    | Remapping Int (Gamepad.Remap.Model String)
    | Playing


type alias Model =
    { game : Game.Model
    , gamepadMaps : Dict String Gamepad.ButtonMap
    , status : Status
    , windowSizeInPixels : Window.Size
    , windowSizeInGameCoordinates : Vector
    , colorations : Array Coloration
    , playersById : Dict Int Player
    }


type alias Input =
    { index : Int
    , speed : Vector
    , aim : Vector
    , fire : Bool
    }



-- helpers


getColoration : Int -> Array Coloration -> Coloration
getColoration index colorations =
    Array.get (index % Array.length colorations) colorations
        |> Maybe.withDefault ( "", "", "" )


gamepadToInput : Gamepad -> Input
gamepadToInput gamepad =
    { index = Gamepad.getIndex gamepad
    , speed = vector (Gamepad.leftX gamepad) (Gamepad.leftY gamepad)
    , aim = vector (Gamepad.rightX gamepad) (Gamepad.rightX gamepad)
    , fire = Gamepad.aIsPressed gamepad
    }


inputToTuple : Input -> ( Vector, Vector, Bool )
inputToTuple input =
    ( input.speed, input.aim, input.fire )


flippedFold : (a -> b -> b) -> List a -> b -> b
flippedFold f list oldB =
    List.foldl f oldB list



-- Game logic


foldEvent : Event -> ( List (Cmd msg), Dict Int Player ) -> ( List (Cmd msg), Dict Int Player )
foldEvent event ( cmds, playersById ) =
    case event of
        ShipExplodes id ->
            ( Ports.playSound "explosion" :: cmds, playersById )

        ShipFires id ->
            ( Ports.playSound "fire" :: cmds, playersById )

        ShipAppears id ->
            ( cmds, playersById )

        ShipActivates id ->
            ( Ports.playSound "spawnEnd" :: cmds, playersById )

        ShipDamagesShip attackerId victimId ->
            (,) cmds <|
                case Dict.get attackerId playersById of
                    Nothing ->
                        playersById

                    Just player ->
                        Dict.insert attackerId { player | score = player.score + 1 } playersById


applyInputToShip : ( Ship, Input ) -> Game.Model -> Game.Model
applyInputToShip ( ship, input ) model =
    Game.update (Game.ControlShip ship (inputToTuple input)) model |> Tuple.first


removeShip : Ship -> Game.Model -> Game.Model
removeShip ship model =
    Game.update (Game.KillShip ship) model |> Tuple.first


addShip : Dict Int Player -> Input -> Game.Model -> Game.Model
addShip playersById input model =
    let
        colorName =
            Dict.get input.index playersById
                |> Maybe.map .coloration
                |> Maybe.map (\( _, _, colorName ) -> colorName)
                |> Maybe.withDefault ""
    in
        Game.update (Game.AddShip input.index colorName) model |> Tuple.first


timeMovesForward : Float -> List Input -> Model -> ( Model, Cmd Msg )
timeMovesForward dt controls model =
    let
        ships =
            Dict.values model.game.shipsById

        findShipInput : Ship -> Maybe ( Ship, Input )
        findShipInput ship =
            controls
                |> List.Extra.find (\control -> control.index == ship.controllerId)
                |> Maybe.map ((,) ship)

        -- Find all ships that HAVE a controller
        controlledShips : List ( Ship, Input )
        controlledShips =
            ships
                |> List.map findShipInput
                |> List.filterMap identity

        -- Find all ships that DO NOT have a controller
        abandonedShips : List Ship
        abandonedShips =
            ships
                |> List.filter (findShipInput >> (==) Nothing)

        -- Find all controls that DO NOT have a ship
        freeInputs : List Input
        freeInputs =
            controls
                |> List.filter (\control -> Dict.get control.index model.game.shipsById == Nothing)

        ( newGame, events ) =
            model.game
                |> flippedFold applyInputToShip controlledShips
                |> flippedFold removeShip abandonedShips
                |> flippedFold (addShip updatedPlayersById) freeInputs
                |> Game.update (Game.Tick dt)

        makePlayer : Input -> Player
        makePlayer control =
            { score = 0
            , controllerId = control.index
            , coloration = getColoration control.index model.colorations
            , isConnected = True
            }

        connectPlayer : Input -> Dict Int Player -> Dict Int Player
        connectPlayer control playersById =
            let
                updatedPlayer =
                    Dict.get control.index playersById |> Maybe.withDefault (makePlayer control)
            in
                Dict.insert control.index { updatedPlayer | isConnected = True } playersById

        disconnectPlayer : Ship -> Dict Int Player -> Dict Int Player
        disconnectPlayer ship playersById =
            case Dict.get ship.controllerId playersById of
                Nothing ->
                    playersById

                Just player ->
                    Dict.insert ship.controllerId { player | isConnected = False } playersById

        updatedPlayersById : Dict Int Player
        updatedPlayersById =
            model.playersById
                |> flippedFold connectPlayer freeInputs
                |> flippedFold disconnectPlayer abandonedShips

        ( soundCmds, scoredPlayersById ) =
            List.foldl foldEvent ( [], updatedPlayersById ) events

        newModel =
            { model
                | game = newGame
                , playersById = scoredPlayersById
            }

        cmd =
            Cmd.batch soundCmds
    in
        ( newModel, cmd )



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
            , windowSizeInGameCoordinates = vector internalCoordinatesWidth internalCoordinatesHeight
        }


animationFrame : Time -> Gamepad.Blob -> Model -> ( Model, Cmd Msg )
animationFrame dt blob model =
    let
        connections =
            List.range 0 3
                |> List.map (Gamepad.getGamepad model.gamepadMaps blob)

        foldConnection connection ( hasUnrecognised, available ) =
            case connection of
                Gamepad.Disconnected ->
                    ( hasUnrecognised, available )

                Gamepad.Unrecognised ->
                    ( True, available )

                Gamepad.Available gamepad ->
                    ( hasUnrecognised, gamepad :: available )

        ( hasUnrecognised, availableGamepads ) =
            List.foldr foldConnection ( False, [] ) connections

        controls =
            --TODO add keyboard.mouse?
            availableGamepads
                |> List.map gamepadToInput
    in
        if hasUnrecognised then
            -- remap
            noCmd model
        else if List.length controls < 1 then
            timeMovesForward dt [] { model | status = NoGamepads }
        else
            timeMovesForward dt controls model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            noCmd model

        WindowResizes windowSize ->
            noCmd <| resizeWindow windowSize model

        AnimationFrameAndGamepads ( dt, blob ) ->
            animationFrame dt blob model


type alias Flags =
    { dateNow : Int
    , gamepadButtonMapsKey : String
    , gamepadButtonMaps : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        seed =
            Random.initialSeed flags.dateNow

        gamepadMaps =
            Gamepad.buttonMapsFromString flags.gamepadButtonMaps |> Result.withDefault Dict.empty

        model =
            { game = Game.init seed
            , gamepadMaps = gamepadMaps
            , status = NoGamepads
            , windowSizeInPixels = { width = 800, height = 600 }
            , windowSizeInGameCoordinates = vector 4 3
            , colorations = Random.step (Random.Array.shuffle View.colorations) seed |> Tuple.first
            , playersById = Dict.empty
            }

        cmd =
            Task.perform WindowResizes Window.size
    in
        ( model, cmd )


viewSplash : Model -> Html msg
viewSplash model =
    case model.status of
        Playing ->
            H.text ""

        NoGamepads ->
            View.splash
                "Haifisch"
                "No gamepads detected, you need at least TWO to play."

        Remapping index remapModel ->
            let
                title =
                    "Remapping gamepad #" ++ toString index

                message =
                    Gamepad.Remap.view remapModel
            in
                View.splash title message


view : Model -> Html Msg
view model =
    H.div
        [ HA.class "ui"
        ]
        [ View.background
        , View.game model.windowSizeInGameCoordinates model.playersById model.game
        , viewSplash model
        , View.scoreboard model.playersById model.game.shipsById
        ]


main =
    H.programWithFlags
        { init = init
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ GamepadPort.gamepad AnimationFrameAndGamepads
                    , Window.resizes WindowResizes
                    ]
        , view = view
        }
