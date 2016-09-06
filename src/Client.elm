module Client exposing (..)


import Keyboard
import Process
import Task
import Time
import View
import Html.App


import GameCommon exposing (Game, Command, EmpireId)
import GameMain
import UI




-- This is supposed to come from interpolating server and browser animationFrame
granularity =
    Time.millisecond * 100




---------------


type alias Model =
    { game : Game
    , ui : UI.Model
    , currentPlayerId : EmpireId
    }



init =
    ( Model GameMain.init UI.init 0
    , Cmd.none
    )




type ServerMessage =
    GameCommand EmpireId Command








type Message
    = Noop
    | Tick
    | UiMessage UI.Message
    | ReceiveFromServer ServerMessage








{- TODO
    For now, just reroute straight back all commands, adding some lag
-}
sendToServer : List Command -> Cmd Message
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



updateGame command model =
    noCmd <| { model | game = GameMain.update command model.game }



update msg model =
    case msg of

        Noop ->
            noCmd model

        Tick ->
            updateGame GameMain.Tick model


        ReceiveFromServer serverMessage ->
            case serverMessage of
                GameCommand empireId command ->
                    updateGame (GameMain.EmpireCommands empireId command) model

        UiMessage uiMessage ->
            let
                ( newUiModel, commands ) =
                    UI.update model.game uiMessage model.ui

                cmd =
                    if List.length commands == 0
                    then Cmd.none
                    else sendToServer commands
            in
                ( { model | ui = newUiModel }, cmd )




view model =
    Html.App.map UiMessage <| View.render model.currentPlayerId model.game model.ui



subscriptions model =
    Sub.batch
        [ Time.every granularity (always Tick)
        , Sub.map UiMessage UI.subscriptions
        ]
