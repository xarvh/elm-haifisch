module Client exposing (..)

import Color exposing (..)
import GameMain as Game
import GameEmpire as Empire
import Keyboard
import Time
import View



type alias Model =
    { game : Game.Game

    -- TODO: right now this refers to an **EmpireId**
    , currentPlayerId : Int
    }


type Message
    = Noop
    | PlayerInput Game.Command
    | Tick Time.Time




-- Game update will be determined by the server, so no point in using the browser's animation frame
granularity =
    100 * Time.millisecond


noCmd model =
    (model, Cmd.none)


update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        Noop ->
            noCmd model

        PlayerInput empireCommand ->
            -- TODO: do not execute the command here, but send it to the server
            noCmd { model | game = Game.update (Game.EmpireCommands model.currentPlayerId empireCommand) model.game }

        -- TODO
        -- MessageFromServer message ->

        Tick epoch ->
            noCmd { model | game = Game.update Game.Tick model.game }



view model =
    View.render model.currentPlayerId model.game



-- shipControls =
--     [   { down = Ship.Turn (Just Ship.Clockwise)
--         , up = Ship.Turn Nothing
--         , keyCodes = [13, 39, 76, 68] -- Enter, Arrow Right, l, d
--         }
--     ,   { down = Ship.Turn (Just Ship.CounterClockwise)
--         , up = Ship.Turn Nothing
--         , keyCodes = [37, 72, 65] -- Arrow Left, h, a
--         }
--     ,   { down = Ship.Thrust True
--         , up = Ship.Thrust False
--         , keyCodes = [32] -- Spacebar
--         }
--     ]




init =
    let
        newGame =
            Game.init

        currentPlayerId = 0
--             Maybe.withDefault 0 <| List.head newGame.players `Maybe.andThen` (fst >> Just)
    in
        ( Model newGame currentPlayerId, Cmd.none )




-- keyPressDispatcher what keyCodeMap keyCode =
--     case keyCodeMap of
--         x :: xs -> if List.member keyCode x.keyCodes then what x else keyPressDispatcher what xs keyCode
--         _ -> Noop --let x = Debug.log "keyCode" keyCode in Noop



subscriptions model =
    Sub.none
--     let
--         key component shipMessages =
--             (component shipMessages) |> Player.CommandShip |> PlayerInput
-- 
--     in
--         Sub.batch
--             [ Time.every granularity Tick
--             , Keyboard.ups <| keyPressDispatcher (key .up) shipControls
--             , Keyboard.downs <| keyPressDispatcher (key .down) shipControls
--             ]
