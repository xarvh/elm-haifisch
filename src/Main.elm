module Main exposing (..)

import Array exposing (Array)
import Common exposing (..)
import Dict exposing (Dict)
import Gamepad exposing (Gamepad)
import Gamepad.Remap exposing (MappableControl(..))
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


-- stuff


messageNoGamepads =
    Message "No gamepads detected. You need at least TWO to play."


gamepadControls =
    [ ( A, "Fire" )
    , ( B, "Fire (different button)" )
    , ( LeftLeft, "Move left" )
    , ( LeftRight, "Move right" )
    , ( LeftUp, "Move up" )
    , ( LeftDown, "Move down" )
    , ( RightLeft, "Aim left" )
    , ( RightRight, "Aim right" )
    , ( RightUp, "Aim up" )
    , ( RightDown, "Aim down" )
    ]



-- types


type Msg
    = OnWindowResizes Window.Size
    | OnGamepads ( Time, Gamepad.Blob )
    | OnRemapMsg Gamepad.Remap.Msg


type Status
    = Message String
    | Remapping (Gamepad.Remap.Model String)
    | Playing


type alias Model =
    { game : Game.Model
    , gamepadMaps : Dict String Gamepad.ButtonMap
    , gamepadLocalStorageKey : String
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
    , speed = vector (Gamepad.leftX gamepad) -(Gamepad.leftY gamepad)
    , aim = vector (Gamepad.rightX gamepad) -(Gamepad.rightY gamepad)
    , fire = Gamepad.aIsPressed gamepad || Gamepad.bIsPressed gamepad
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


updateAnimationFrame : Time -> Gamepad.Blob -> Model -> ( Model, Cmd Msg )
updateAnimationFrame dt blob model =
    let
        connections =
            List.range 0 3
                |> List.map (Gamepad.getGamepad model.gamepadMaps blob)

        foldConnection connection ( unrecognised, available ) =
            case connection of
                Gamepad.Disconnected ->
                    ( unrecognised, available )

                Gamepad.Unrecognised ->
                    -- TODO AAAARGH!
                    ( Just 0, available )

                Gamepad.Available gamepad ->
                    ( unrecognised, gamepad :: available )

        ( unrecognised, availableGamepads ) =
            List.foldr foldConnection ( Nothing, [] ) connections

        controls =
            --TODO add keyboard.mouse?
            availableGamepads
                |> List.map gamepadToInput
    in
        case unrecognised of
            Just gamepadIndex ->
                case model.status of
                    Remapping _ ->
                        noCmd model

                    _ ->
                        noCmd { model | status = Remapping <| Gamepad.Remap.init gamepadIndex gamepadControls }

            Nothing ->
                if List.length controls < 1 then
                    timeMovesForward dt [] { model | status = messageNoGamepads }
                else
                    timeMovesForward dt controls model


updateRemap : Gamepad.Remap.Msg -> Gamepad.Remap.Model String -> Model -> ( Model, Cmd Msg )
updateRemap remapMsg remapModel model =
    case Gamepad.Remap.update remapMsg remapModel of
        Gamepad.Remap.StillOpen newModel ->
            noCmd { model | status = Remapping newModel }

        Gamepad.Remap.Error message ->
            noCmd { model | status = Message message }

        Gamepad.Remap.Configured id buttonMap ->
            let
                gamepadMaps =
                    model.gamepadMaps |> Dict.insert id buttonMap

                cmd =
                    LocalStoragePort.set model.gamepadLocalStorageKey (Gamepad.buttonMapsToString gamepadMaps)

                status =
                    Message "Configured!"
            in
                ( { model | status = status, gamepadMaps = gamepadMaps }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnWindowResizes windowSize ->
            noCmd <| resizeWindow windowSize model

        OnGamepads ( dt, blob ) ->
            updateAnimationFrame dt blob model

        OnRemapMsg remapMsg ->
            case model.status of
                Remapping remapModel ->
                    updateRemap remapMsg remapModel model

                _ ->
                    noCmd model


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
            , gamepadLocalStorageKey = flags.gamepadButtonMapsKey
            , status = messageNoGamepads
            , windowSizeInPixels = { width = 800, height = 600 }
            , windowSizeInGameCoordinates = vector 4 3
            , colorations = Random.step (Random.Array.shuffle View.colorations) seed |> Tuple.first
            , playersById = Dict.empty
            }

        cmd =
            Task.perform OnWindowResizes Window.size
    in
        ( model, cmd )


viewSplash : Model -> Html msg
viewSplash model =
    case model.status of
        Playing ->
            H.text ""

        Message message ->
            View.splash
                "Haifisch"
                message

        Remapping remapModel ->
            let
                title =
                    "Remapping gamepad #" ++ toString (Gamepad.Remap.gamepadIndex remapModel)

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


subscriptions model =
    Sub.batch
        [ Window.resizes OnWindowResizes
        , GamepadPort.gamepad OnGamepads
        , Gamepad.Remap.subscriptions GamepadPort.gamepad |> Sub.map OnRemapMsg
        ]


main =
    H.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
