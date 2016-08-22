module UI exposing (..)


import GameMain as Game
import GameEmpire as Empire




-- MODEL


type alias Model =
    { game : Game.Game

    -- TODO: right now this refers to an **EmpireId**
    , currentPlayerId : Int
    }



init =
    ( Model Game.init 0, Cmd.none )





-- UPDATE


type Message
    = Noop
    | PlayerInput Game.Command
    | Tick



noCmd model =
    (model, Cmd.none)


update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        Noop ->
            noCmd model

        PlayerInput empireCommand ->
            let
                x = Debug.log "c" empireCommand
            -- TODO: do not execute the command here, but send it to the server
            in
                noCmd { model | game = Game.update (Game.EmpireCommands model.currentPlayerId empireCommand) model.game }

        -- TODO
        -- MessageFromServer message ->

        Tick ->
            noCmd { model | game = Game.update Game.Tick model.game }
