module Client exposing (..)

import Process
import Task
import Time
import Html.App
import GameCommon exposing (Game, Command, EmpireId)
import GameMain
import UiMain


-- This is supposed to come from interpolating server and browser animationFrame


granularity =
    Time.millisecond * 100



---------------


type alias Model =
    { game : Game
    , ui : UiMain.Model
    , currentPlayerId : EmpireId
    }


init seed =
    ( Model (GameMain.init seed) UiMain.init 0
    , Cmd.none
    )


type ServerMessage
    = GameCommand EmpireId Command


type Msg
    = Noop
    | Tick
    | ToUiMainMsg UiMain.Msg
    | ReceiveFromServer ServerMessage



{- TODO
   For now, just reroute straight back all commands, adding some lag
-}


sendToServer : List Command -> Cmd Msg
sendToServer commands =
    let
        playerId =
            0

        tagger command _ =
            ReceiveFromServer <| GameCommand playerId command

        task =
            Process.sleep <| Time.millisecond * 100

        commandToCmd command =
            Task.perform (tagger command) (tagger command) task
    in
        Cmd.batch <| List.map commandToCmd commands


noCmd model =
    ( model, Cmd.none )


updateGameAndUi command model =
    let
        ( newGame, notifications ) =
            GameMain.update command model.game

        newUi =
            List.foldl UiMain.updateForNotification model.ui notifications
    in
        noCmd { model | game = newGame, ui = newUi }


update msg model =
    case msg of
        Noop ->
            noCmd model

        Tick ->
            updateGameAndUi GameMain.Tick model

        ReceiveFromServer serverMessage ->
            case serverMessage of
                GameCommand empireId command ->
                    updateGameAndUi (GameMain.EmpireCommands empireId command) model

        ToUiMainMsg nestedMsg ->
            let
                ( newUiModel, commands ) =
                    UiMain.update nestedMsg model.game model.ui

                cmd =
                    if List.length commands == 0 then
                        Cmd.none
                    else
                        sendToServer commands
            in
                ( { model | ui = newUiModel }, cmd )


view model =
    Html.App.map ToUiMainMsg <| UiMain.view model.currentPlayerId model.game model.ui


subscriptions model =
    Sub.batch
        [ Time.every granularity (always Tick)
        , Sub.map ToUiMainMsg UiMain.subscriptions
        ]
