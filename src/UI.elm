module UI exposing (..)


import GameCommon as G exposing
    ( Vector
    , vector
    , starSystemOuterRadius
    , normalizeBox
    , Command
    )

import GameEmpire as Empire
import GameMain as Game

import Html.Events
import Json.Decode as Json exposing ((:=))
import Keyboard
import Svg
import Svg.Events
import SvgMouse

import Math.Vector2 as V



-- MODEL


type SelectionType
    = FleetSelection


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

    , ctrl : Bool
    , shift : Bool
    }



init =
    { selectionType = FleetSelection
    , selectedIds = [1..99]
    , selectionBox = Nothing
    , starSystemMousePosition = vector 0 0
    , ctrl = False
    , shift = False
    }




-- SELECTION

select selectionType selectedIds model =
    { model
    | selectionType = FleetSelection
    , selectedIds = selectedIds
    , selectionBox = Nothing
    }


selectBox game start end model =
    let
        (x, y, x', y') =
            normalizeBox start end

        isInBox ship =
            let
                (sx, sy) = V.toTuple ship.currentPosition
            in
                sx >= x && sx <= x'
                && sy >= y && sy <= y'

        fm fleet =
            if List.any isInBox fleet.ships
             --   && fleet.empireId == currentPlayerId
            then Just fleet.id
            else Nothing

    in
        { model
        | selectionType = FleetSelection
        , selectedIds = List.filterMap fm game.fleets
        , selectionBox = Nothing
        }






-- COMMAND

command pos model =
    let
        queueMode = if model.shift then G.Append else G.Replace
    in
        ( model, [G.FleetCommand model.selectedIds queueMode <| G.ThrustTo pos] )



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
                            select FleetSelection [] model
                        Just startPosition ->
                            selectBox game startPosition pos model


-- KEYBOARD

manageKeys status keyCode model =
    noCmd <| case keyCode of
        16 -> { model | shift = status }
        17 -> { model | ctrl = status }
        _ -> model



-- UPDATE


type Message
    = StarSystemMouseMove MouseButtonIndex Vector
    | StarSystemMousePress MouseButtonIndex Vector
    | StarSystemMouseRelease MouseButtonIndex Vector

    | UserClicksFleet Int MouseButtonIndex Vector

    | KeyPress Keyboard.KeyCode
    | KeyRelease Keyboard.KeyCode



update : Game.Game -> Message -> Model -> (Model, List Command)
update game message model =
    case message of

        KeyPress key ->
            manageKeys True key model

        KeyRelease key ->
            manageKeys False key model

        StarSystemMouseMove button pos ->
            noCmd { model | starSystemMousePosition = pos }

        StarSystemMousePress button pos ->
            manageStarSystemMouse game button MousePress pos model

        StarSystemMouseRelease button pos ->
            manageStarSystemMouse game button MouseRelease pos model

        UserClicksFleet fleetId button pos ->
            noCmd <| select FleetSelection [fleetId] model


-- SUBS


subscriptions =
    Sub.batch
        [ Keyboard.downs KeyPress
        , Keyboard.ups KeyRelease
        ]
