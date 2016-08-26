module UI exposing (..)


-- import GameCommon exposing (Pos)
import GameEmpire as Empire
import GameMain as Game

import Html.Events
import Json.Decode as Json exposing ((:=))
import Svg
import Svg.Events
import SvgMouse



-- MODEL



type SelectionType
    = ShipSelection





type alias Model =
    { game : Game.Game

    -- TODO: right now this refers to an **EmpireId**
    , currentPlayerId : Int

    , selectionType : SelectionType
    , selectedIds : List Int
    }



init =
    ( Model Game.init 0 ShipSelection []
    , Cmd.none
    )






-- SELECTION

select selectionType selectedIds model =
    { model
    | selectionType = ShipSelection
    , selectedIds = selectedIds
    }




-- Events management


mouseMoveOn : String -> Svg.Attribute Message
mouseMoveOn selector =
    let
        dispatcher clientX clientY =
            MouseMove <| SvgMouse.transform selector clientX clientY

        position =
            Json.object2 dispatcher ("clientX" := Json.int) ("clientY" := Json.int)

    in
        Svg.Events.on "mousemove" position




onEventCooked eventName tagger =
    Html.Events.onWithOptions eventName { stopPropagation = True, preventDefault = True } (Json.succeed tagger)

onLeftClickCooked =
    onEventCooked "click"

onRightClickCooked =
    onEventCooked "contextmenu"






-- UPDATE


type Message
    = Noop
--     | PlayerInput Game.Command
    | MouseMove (Float, Float)
    | UserClicksShip Int
    | UserIssuesCommand
    | UserSelectsNone
    | Tick



noCmd model =
    (model, Cmd.none)


update : Message -> Model -> (Model, Cmd Message)
update message model =
    case message of
        Noop ->
            noCmd model

--         PlayerInput empireCommand ->
--             let
--                 x = Debug.log "c" empireCommand
--             -- TODO: do not execute the command here, but send it to the server
--             in
--                 noCmd { model | game = Game.update (Game.EmpireCommands model.currentPlayerId empireCommand) model.game }

        -- TODO
        -- MessageFromServer message ->

        MouseMove ( x, y ) ->
            noCmd model

        UserClicksShip shipId ->
            noCmd <| select ShipSelection [shipId] model

        UserIssuesCommand ->
            let
                q = Debug.log "qq" "Command!"
            in
                noCmd model

        UserSelectsNone ->
            noCmd <| select ShipSelection [] model

        Tick ->
            noCmd { model | game = Game.update Game.Tick model.game }
