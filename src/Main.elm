module Main exposing (..)

import Array exposing (Array)
import Common exposing (..)
import Dict exposing (Dict)
import Gamepad exposing (Gamepad)
import Game exposing ((|>>))
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.App
import List.Extra
import Ports
import Random
import Random.Array
import Task
import Window
import Time exposing (Time)
import View


-- Model


type alias Model =
    { game : Game.Model
    , hasGamepads : Bool
    , windowSizeInPixels : Window.Size
    , windowSizeInGameCoordinates : Vector
    , colorations : Array View.Coloration
    , playersById : Dict Int { score : Int, coloration : View.Coloration }
    }



-- Gamepad guessing


anyButton buttons =
    List.any fst buttons


guessGamepad gamepad =
    case gamepad.axes of
        -- Some Xbox 360 in Chrom
        xLeftStick :: yLeftStick :: xRightStick :: yRightStick :: [] ->
            ( vector xLeftStick yLeftStick, vector xRightStick yRightStick, anyButton gamepad.buttons )

        xLeftStick :: yLeftStick :: leftTrigger :: xRightStick :: yRightStick :: yRightTrigger :: [] ->
            ( vector xLeftStick yLeftStick, vector xRightStick yRightStick, anyButton gamepad.buttons )

        -- Xbox 360 in Firefox
        xLeftStick :: yLeftStick :: leftTrigger :: xRightStick :: yRightStick :: yRightTrigger :: xPad :: yPad :: [] ->
            ( vector xLeftStick yLeftStick, vector xRightStick yRightStick, anyButton gamepad.buttons )

        _ ->
            ( vector 0 0, vector 0 0, False )



-- Game logic


gamepadToCommand gamepad =
    guessGamepad gamepad


gamepadShip ( ship, gamepad ) model =
    Game.update (Game.ControlShip ship <| guessGamepad gamepad) model |> fst


removeShip ship model =
    Game.update (Game.KillShip ship) model |> fst


addShip gamepad model =
    Game.update (Game.AddShip gamepad.index) model |> fst


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

        ( newGame, events ) =
            model.game
                |> apply gamepadShip shipsAndGamepads
                |> apply removeShip shipsWithoutGamepad
                |> apply addShip gamepadsWithoutShip
                |> Game.update (Game.Tick dt)

        makePlayer gamepad =
            { score = 0
            , coloration =
                Array.get (gamepad.index % Array.length model.colorations) model.colorations
                    |> Maybe.withDefault ( "", "" )
            }

        updatePlayer gamepad playersById =
            case Dict.get gamepad.index playersById of
                Nothing ->
                    Dict.insert gamepad.index (makePlayer gamepad) playersById

                Just player ->
                    model.playersById

        updatedPlayersById =
            List.foldl updatePlayer model.playersById gamepadsWithoutShip

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

        ( soundCmds, scoredPlayersById ) =
            List.foldl foldEvent ( [], updatedPlayersById ) events
    in
        { model
            | game = newGame
            , hasGamepads = gamepads /= []
            , playersById = scoredPlayersById
        }
            ! soundCmds


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
    let
        seed =
            Random.initialSeed dateNow
    in
        { game = Game.init seed
        , hasGamepads = False
        , windowSizeInPixels = { width = 800, height = 600 }
        , windowSizeInGameCoordinates = vector 4 3
        , colorations = Random.step (Random.Array.shuffle View.colorations) seed |> fst
        , playersById = Dict.empty
        }
            ! [ Task.perform identity WindowResizes Window.size ]


view : Model -> Html Msg
view model =
    H.div
        [ HA.class "ui"
        ]
        [ View.background
        , View.game model.windowSizeInGameCoordinates model.playersById model.game
        , View.splash model.hasGamepads
        , View.scoreboard model.playersById model.game.shipsById
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
