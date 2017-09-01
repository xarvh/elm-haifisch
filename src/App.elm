module App exposing (..)

import Dict exposing (Dict)
import Game
import Gamepad
import GamepadPort
import Html exposing (..)
import Input
import LocalStoragePort
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import MousePort
import Random
import Task
import Time exposing (Time)
import View
import Window


-- types


type alias Config =
    { maybeInputConfig : Maybe Input.Config
    , gamepadDatabase : Gamepad.Database
    }


type alias Model =
    { input : Input.Model
    , game : Game.Model
    , windowSizeInPixels : Window.Size
    , windowSizeInGameCoordinates : Vec2
    }


type Msg
    = OnAnimationFrame ( Time, Gamepad.Blob ) -- This one is called directly by Config
    | OnInputMsg Input.Msg
    | OnWindowResizes Window.Size



-- init


init : ( Model, Cmd Msg )
init =
    ( { input = Input.init
      , game = Game.init (Random.initialSeed 0)
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
            Game.worldRadius * 2.1

        internalCoordinatesWidth =
            toFloat sizeInPixels.width * internalCoordinatesHeight / toFloat sizeInPixels.height
    in
        { model
            | windowSizeInPixels = sizeInPixels
            , windowSizeInGameCoordinates = vec2 internalCoordinatesWidth internalCoordinatesHeight
        }


updateAnimationFrame : Config -> Time -> Gamepad.Blob -> Model -> ( Model, Cmd Msg )
updateAnimationFrame config dt blob model =
    let
        oldGame =
            model.game

        {-
           ( playersMinusInputs, inputModel, players ) =
               Input.updatePlayersInput
                   { gamepads = Gamepad.getGamepads config.gamepadDatabase blob
                   , maybeConfig = config.maybeInputConfig
                   }
                   dt
                   model.input
                   oldGame.players

           addNewPlayer =
               -- if there are more inputs than players
               if playersMinusInputs < 0 then
                   Game.addPlayer >> Tuple.second
               else
                   identity

           game =
               { oldGame | players = players } |> addNewPlayer
        -}
        ( game, events ) =
            Game.tick dt oldGame
    in
        noCmd { model | game = game }


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update config msg model =
    case msg of
        OnAnimationFrame ( dt, blob ) ->
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
        [ View.game model.windowSizeInGameCoordinates Dict.empty model.game
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes OnWindowResizes
        , Input.subscriptions model.input |> Sub.map OnInputMsg
        ]
