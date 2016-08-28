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


type MouseButtonIndex
    = MouseLeft
    | MouseRight
    | MouseMid


type MouseButtonMovement
    = MousePress
    | MouseRelease



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



-- STAR SYSTEM MOUSE MANAGER


starSystemSvgId =
    "starsystem-view"


decodeStarSystemMouse tagger =
    let
        toVector (x, y) =
            vector x y

        decodeMouseButtons which button =
            if which == 1 || button == 0
            then MouseLeft
            else if which == 3 || button == 2
                then MouseRight
                else MouseMid

        mapper clientX clientY which button =
            tagger (decodeMouseButtons which button) <| toVector <| SvgMouse.transform ("svg#" ++ starSystemSvgId) clientX clientY
    in
        Json.object4 mapper ("clientX" := Json.int) ("clientY" := Json.int) ("which" := Json.int) ("button" := Json.int)


onEventCooked eventName tagger =
    Html.Events.onWithOptions eventName { stopPropagation = True, preventDefault = True } <| decodeStarSystemMouse tagger




noCmd model =
    (model, [])


manageStarSystemMouse game mouseButton mouseButtonDirection pos model =
    case mouseButton of

        -- Mid mouse is not used
        MouseMid ->
            noCmd model

        -- Issue command on click
        MouseRight ->
            case mouseButtonDirection of
                MousePress ->
                    noCmd model

                MouseRelease ->
                    command pos model

        -- Start / finish selection box
        MouseLeft ->
            noCmd <| case mouseButtonDirection of

                MousePress ->
                    case model.selectionBox of
                        Nothing ->
                            { model | selectionBox = Just pos }

                        -- if for some reason a marking box is already active, do not reset it
                        Just startPosition ->
                            model

                MouseRelease ->
                    case model.selectionBox of
                        Nothing ->
                            select ShipSelection [] model
                        Just startPosition ->
                            selectBox game startPosition pos model




-- UPDATE


type Message
    = StarSystemMouseMove MouseButtonIndex Vector
    | StarSystemMousePress MouseButtonIndex Vector
    | StarSystemMouseRelease MouseButtonIndex Vector

    | UserClicksShip Int MouseButtonIndex Vector




update : Game.Game -> Message -> Model -> (Model, List Command)
update game message model =
    case message of

        StarSystemMouseMove button pos ->
            noCmd { model | starSystemMousePosition = pos }

        StarSystemMousePress button pos ->
            manageStarSystemMouse game button MousePress pos model

        StarSystemMouseRelease button pos ->
            manageStarSystemMouse game button MouseRelease pos model

        UserClicksShip shipId button pos ->
            noCmd <| select ShipSelection [shipId] model
