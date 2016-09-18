module UI exposing (..)


import GameCommon as G exposing
    ( Game
    , Vector
    , Id
    , vector
    , starSystemOuterRadius
    , normalizeBox
    , Command
    )

import GameMain

import Html.Events
import Json.Decode as Json exposing ((:=))
import Keyboard
import SvgMouse
import Set

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
    { selectionType : SelectionType
    , selectedIds : List Int -- TODO: this should be a set (I assume it would be faster on membership checks?)

    , selectionBox : Maybe Vector
    , starSystemMousePosition : Vector

    -- Just fleetId, idOfShipsToBeExtracted
    , splittingFleet : Maybe (Id, Set.Set Id)

    , ctrl : Bool
    , shift : Bool
    }



init =
    { selectionType = FleetSelection
    , selectedIds = [1..99]
    , selectionBox = Nothing
    , starSystemMousePosition = vector 0 0
    , splittingFleet = Nothing
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


queueMode model =
    if model.shift then G.Append else G.Replace


command pos model =
    ( model, [G.FleetCommand model.selectedIds (queueMode model) <| G.ThrustTo pos] )



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




-- COMMANDS FACTORIES


mergeFleets : Game -> Model -> List G.Command
mergeFleets game model =
    case List.filter (\fleet -> List.member fleet.id model.selectedIds) game.fleets |> List.map .id of

        first :: others ->
            [ G.FleetCommand others (queueMode model) (G.MergeWith first) ]

        [] ->
            []




-- KEYBOARD

manageKeys game status keyCode model =
    case keyCode of
        16 -> noCmd { model | shift = status }
        17 -> noCmd { model | ctrl = status }

        -- space bar
        32 -> ( model, if status then [G.TogglePause] else [] )

        -- M
        77 -> ( model, if status then mergeFleets game model else [] )


        _ -> let a = Debug.log "key" keyCode in noCmd model



-- UPDATE


type Msg
    = StarSystemMouseMove MouseButtonIndex Vector
    | StarSystemMousePress MouseButtonIndex Vector
    | StarSystemMouseRelease MouseButtonIndex Vector

    | UserClicksFleet Int MouseButtonIndex Vector

    | KeyPress Keyboard.KeyCode
    | KeyRelease Keyboard.KeyCode

    | UserClicksFleetSplit Id
    | UserClicksShipSplit Id



update : Game -> Msg -> Model -> (Model, List Command)
update game message model =
    case message of

        KeyPress key ->
            manageKeys game True key model

        KeyRelease key ->
            manageKeys game False key model

        StarSystemMouseMove button pos ->
            noCmd { model | starSystemMousePosition = pos }

        StarSystemMousePress button pos ->
            manageStarSystemMouse game button MousePress pos model

        StarSystemMouseRelease button pos ->
            manageStarSystemMouse game button MouseRelease pos model

        UserClicksFleet fleetId button pos ->
            noCmd <| select FleetSelection [fleetId] model

        UserClicksFleetSplit clickedFleetId ->
            case Maybe.withDefault (-1, Set.empty) model.splittingFleet of
                ( selectedFleetId, shipIds ) ->
                    if selectedFleetId /= clickedFleetId
                    then
                        noCmd { model | splittingFleet = Just (clickedFleetId, Set.empty) }
                    else
                        let
                            newModel = { model | splittingFleet = Nothing }
                            -- TODO: send command to split fleet
                            cmd = if Set.isEmpty shipIds then [] else []
                        in
                            ( newModel, cmd )

        UserClicksShipSplit shipId ->
            noCmd <| case model.splittingFleet of
                Nothing -> model
                Just ( fId, shipIds ) ->
                    { model | splittingFleet = Just ( fId, (if Set.member shipId shipIds then Set.remove else Set.insert) shipId shipIds ) }


-- SUBS


subscriptions =
    Sub.batch
        [ Keyboard.downs KeyPress
        , Keyboard.ups KeyRelease
        ]
