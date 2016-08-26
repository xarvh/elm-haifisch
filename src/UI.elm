module UI exposing (..)


import GameCommon exposing (Vector, vector, Command (ShipMove))
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
    -- TODO: right now this refers to an **EmpireId**

    { selectionType : SelectionType
    , selectedIds : List Int -- TODO: this should be a set (I assume it would be faster on membership checks?)

    , starSystemMousePosition : Vector
    }



init =
    { selectionType = ShipSelection
    , selectedIds = [1..99]
    , starSystemMousePosition = vector 0 0
    }



-- SELECTION

select selectionType selectedIds model =
    { model
    | selectionType = ShipSelection
    , selectedIds = selectedIds
    }




-- COMMAND

command pos model =
    ( model, [ShipMove model.selectedIds pos] )



-- Mouse management


starSystemSvgId =
    "starsystem-view"


decodeStarSystemMousePosition tagger =
    let
        toVector (x, y) =
            vector x y

        mapper clientX clientY =
            tagger <| toVector <| SvgMouse.transform ("svg#" ++ starSystemSvgId) clientX clientY
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
    = MouseMove Vector
    | UserClicksShip Int Vector
    | UserIssuesCommand Vector
    | UserSelectsNone Vector



noCmd model =
    (model, [])


update : Message -> Model -> (Model, List Command)
update message model =
    case message of
        MouseMove pos ->
            noCmd { model | starSystemMousePosition = pos }

        UserClicksShip shipId pos ->
            noCmd <| select ShipSelection [shipId] model

        UserIssuesCommand pos ->
            command pos model

        UserSelectsNone pos ->
            noCmd <| select ShipSelection [] model
