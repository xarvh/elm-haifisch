module UI exposing (..)


import GameCommon exposing (Pos, pos)
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

    , starSystemMousePosition : Pos
    }



init =
    let
        model =
            { game = Game.init
            , currentPlayerId = 0
            , selectionType = ShipSelection
            , selectedIds = []
            , starSystemMousePosition = pos 0 0
            }
    in
        ( model, Cmd.none )




-- SELECTION

select selectionType selectedIds model =
    { model
    | selectionType = ShipSelection
    , selectedIds = selectedIds
    }




-- Mouse management


starSystemSvgId =
    "starsystem-view"


decodeStarSystemMousePosition tagger =
    let
        toPos (x, y) =
            pos x y

        mapper clientX clientY =
            tagger <| toPos <| SvgMouse.transform ("svg#" ++ starSystemSvgId) clientX clientY
    in
        Json.object2 mapper ("clientX" := Json.int) ("clientY" := Json.int)





onMouseMove tagger =
    Svg.Events.on "mousemove" <| decodeStarSystemMousePosition tagger


onEventCooked eventName tagger =
    Html.Events.onWithOptions eventName { stopPropagation = True, preventDefault = True } <| decodeStarSystemMousePosition tagger


onLeftClickCooked =
    onEventCooked "click"


onRightClickCooked =
    onEventCooked "contextmenu"






-- UPDATE


type Message
    = Noop
--     | PlayerInput Game.Command
    | MouseMove Pos
    | UserClicksShip Int Pos
    | UserIssuesCommand Pos
    | UserSelectsNone Pos
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

        MouseMove pos ->
            noCmd { model | starSystemMousePosition = pos }

        UserClicksShip shipId pos ->
            noCmd <| select ShipSelection [shipId] model

        UserIssuesCommand pos ->
                noCmd model

        UserSelectsNone pos ->
            noCmd <| select ShipSelection [] model

        Tick ->
            noCmd { model | game = Game.update Game.Tick model.game }
