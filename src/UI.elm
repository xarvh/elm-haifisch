module UI exposing (..)


import GameCommon exposing
    ( Vector
    , vector
    , starSystemOuterRadius
    , normalizeBox
    , Command (ShipMove)
    )

import GameEmpire as Empire
import GameMain as Game

import Html.Events
import Json.Decode as Json exposing ((:=))
import Svg
import Svg.Events
import SvgMouse

import Math.Vector2 as V


-- MODEL



type SelectionType
    = ShipSelection





type alias Model =
    -- TODO: right now this refers to an **EmpireId**

    { selectionType : SelectionType
    , selectedIds : List Int -- TODO: this should be a set (I assume it would be faster on membership checks?)

    , selectionBox : Maybe Vector
    , starSystemMousePosition : Vector
    }



init =
    { selectionType = ShipSelection
    , selectedIds = [1..99]
    , selectionBox = Nothing
    , starSystemMousePosition = vector 0 0
    }



-- SELECTION

select selectionType selectedIds model =
    { model
    | selectionType = ShipSelection
    , selectedIds = selectedIds
    , selectionBox = Nothing
    }


selectBox game start end model =
    let
        (x, y, x', y') =
            normalizeBox start end

        fm ship =
            let
                sx = V.getX ship.position
                sy = V.getY ship.position
            in
                if sx >= x && sx <= x'
                && sy >= y && sy <= y'
              --   && ship.empireId == currentPlayerId
                then Just ship.id
                else Nothing

    in
        { model
        | selectionType = ShipSelection
        , selectedIds = List.filterMap fm game.ships
        , selectionBox = Nothing
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


onLeftDownCooked =
    onEventCooked "mousedown"

onLeftClickCooked =
    onEventCooked "click"

onRightClickCooked =
    onEventCooked "contextmenu"






-- UPDATE


type Message
    = MouseMove Vector
    | UserStartsSelectionBox Vector
    | UserClicksShip Int Vector
    | UserIssuesCommand Vector
    | UserLeftClicks Vector



noCmd model =
    (model, [])


update : Game.Game -> Message -> Model -> (Model, List Command)
update game message model =
    case message of
        MouseMove pos ->
            noCmd { model | starSystemMousePosition = pos }

        UserStartsSelectionBox pos ->
            noCmd { model | selectionBox = Just pos }

        UserClicksShip shipId pos ->
            noCmd <| select ShipSelection [shipId] model

        UserIssuesCommand pos ->
            command pos model

        UserLeftClicks endPosition ->
            noCmd <| case model.selectionBox of
                Nothing -> select ShipSelection [] model
                Just startPosition ->
                    selectBox game startPosition endPosition model

