module StarSystemUi exposing (..)

import Dict
import GameCommon as G
    exposing
        ( Game
        , Id
        , Fleet
        , starSystemOuterRadius
        , vectorToString
        , normalizeBox
        , Vector
        , vector
        )
import FleetView
import Html.Events
import Math.Vector2 as V
import Json.Decode as Json exposing ((:=))
import Set
import Svg
import Svg.Attributes as A
import SvgMouse
import String
import UiCommon as Ui


type alias Svg =
    Svg.Svg Msg



-- MODEL


type alias Model =
    { selectionBox : Maybe Vector
    , mousePosition : Vector
    }


init =
    Model Nothing (vector 0 0)


type Msg
    = MouseMove Ui.MouseButtonIndex Vector
    | MousePress Ui.MouseButtonIndex Vector
    | MouseRelease Ui.MouseButtonIndex Vector
    | UserClicksFleet Int Ui.MouseButtonIndex Vector


update : Msg -> Game -> Ui.UiShared a -> Model -> ( Model, Ui.Selection, List G.Command )
update msg game uiShared model =
    case msg of
        MouseMove button pos ->
            ( { model | mousePosition = pos }, uiShared.selection, [] )

        MousePress button pos ->
            manageMouse button Ui.MousePress game uiShared model

        MouseRelease button pos ->
            manageMouse button Ui.MouseRelease game uiShared model

        UserClicksFleet fleetId button pos ->
            case button of
                Ui.MouseRight ->
                    ( model, uiShared.selection, targetFleetCommand fleetId game uiShared )

                _ ->
                    select (Ui.FleetSelection <| Set.singleton fleetId) model


select selection model =
    ( { model | selectionBox = Nothing }, selection, [] )


targetFleetCommand : G.Id -> Game -> Ui.UiShared a -> List G.Command
targetFleetCommand targetFleetId game uiShared =
    [ G.FleetCommand (Ui.fleetIds uiShared) (Ui.queueMode uiShared) (G.Attack targetFleetId) ]



-- MOUSE MANAGER


manageMouse mouseButton mouseButtonDirection game uiShared model =
    let
        doNothing =
            ( model, uiShared.selection, [] )
    in
        case mouseButton of
            -- Mid mouse is not used
            Ui.MouseMid ->
                doNothing

            -- Issue command on click
            Ui.MouseRight ->
                case mouseButtonDirection of
                    Ui.MousePress ->
                        doNothing

                    Ui.MouseRelease ->
                        command game uiShared model

            -- Start / finish selection box
            Ui.MouseLeft ->
                case mouseButtonDirection of
                    Ui.MousePress ->
                        case model.selectionBox of
                            Nothing ->
                                ( { model | selectionBox = Just model.mousePosition }, uiShared.selection, [] )

                            -- if for some reason a marking box is already active, do not reset it
                            Just startPosition ->
                                doNothing

                    Ui.MouseRelease ->
                        let
                            selection =
                                case model.selectionBox of
                                    Nothing ->
                                        Ui.NoSelection

                                    Just startPosition ->
                                        boxSelection startPosition model.mousePosition game
                        in
                            select selection model


boxSelection start end game =
    let
        ( x, y, x', y' ) =
            normalizeBox start end

        isInBox ship =
            let
                ( sx, sy ) =
                    V.toTuple ship.currentPosition
            in
                sx
                    >= x
                    && sx
                    <= x'
                    && sy
                    >= y
                    && sy
                    <= y'

        folder id fleet set =
            if
                List.any isInBox fleet.ships
                --   && fleet.empireId == currentPlayerId
            then
                Set.insert id set
            else
                set

        ids =
            Dict.foldl folder Set.empty game.fleets

        selection =
            if Set.isEmpty ids then
                Ui.NoSelection
            else
                Ui.FleetSelection ids
    in
        selection


command game uiShared model =
    let
        commands =
            case uiShared.selection of
                Ui.NoSelection ->
                    []

                Ui.FleetSelection fleetIds ->
                    [ G.FleetCommand fleetIds (Ui.queueMode uiShared) (G.ThrustTo model.mousePosition) ]
    in
        ( model, uiShared.selection, commands )



-------------------------------------------------
-- TODO: once StarSystemUi becomes full-screen coordinate transformation can be simplified


starSystemSvgId =
    "starsystem-view"


decodeStarSystemMouse tagger =
    let
        toVector ( x, y ) =
            vector x y

        decodeMouseButtons which button =
            if which == 1 || button == 0 then
                Ui.MouseLeft
            else if which == 3 || button == 2 then
                Ui.MouseRight
            else
                Ui.MouseMid

        mapper clientX clientY which button =
            tagger (decodeMouseButtons which button) <| toVector <| SvgMouse.transform ("svg#" ++ starSystemSvgId) clientX clientY
    in
        Json.object4 mapper ("clientX" := Json.int) ("clientY" := Json.int) ("which" := Json.int) ("button" := Json.int)


onEventCooked eventName tagger =
    Html.Events.onWithOptions eventName { stopPropagation = True, preventDefault = True } <| decodeStarSystemMouse tagger



-- Fleet Commands Traces


symbolDot pos =
    Svg.circle
        [ A.cx <| toString <| V.getX pos
        , A.cy <| toString <| V.getY pos
        , A.r "0.005"
        , A.fill "#0b0"
        ]
        []


symbolMerge pos =
    Svg.g
        [ A.transform <| "translate(" ++ G.vectorToString pos ++ ") scale(0.008)"
        ]
        [ Svg.polygon
            [ A.points "-1,1  1,1  1,-1  -1,-1"
            , A.fill "#0db"
            ]
            []
        ]


symbolAttack pos =
    Svg.g
        [ A.transform <| "translate(" ++ G.vectorToString pos ++ ") scale(0.016)"
        ]
        [ Svg.polygon
            [ A.points "0,0.43 -0.5,-0.5 0.5,-0.5"
            , A.fill "#f00"
            ]
            []
        ]


fleetPosition fleetId game =
    Dict.get fleetId game.fleets
        |> (flip Maybe.andThen) (\fleet -> List.head fleet.ships)
        |> Maybe.map .currentPosition
        |> Maybe.withDefault (vector 0 0)


drawFleetCommand : Game -> G.Vector -> G.FleetCommand -> ( G.Vector, List (Svg.Svg Msg) )
drawFleetCommand game start shipCommand =
    let
        ( symbol, end ) =
            case shipCommand of
                G.ThrustTo end ->
                    ( symbolDot, end )

                G.MergeWith targetFleetId ->
                    ( symbolMerge, fleetPosition targetFleetId game )

                G.Attack targetFleetId ->
                    ( symbolAttack, fleetPosition targetFleetId game )

        difference =
            V.sub end start

        length =
            V.length difference

        direction =
            V.scale (1 / length) difference

        baseStepLength =
            0.025

        numberOfSteps =
            round <| length / baseStepLength

        actualStepLength =
            length / toFloat numberOfSteps

        position n =
            direction
                |> V.scale (toFloat n * actualStepLength)
                |> V.sub end

        symbols =
            List.map (position >> symbol) [0..numberOfSteps - 1]
    in
        ( end, symbols )


drawFleetCommandQueue : Id -> Game -> Fleet -> List Svg
drawFleetCommandQueue asViewedByPlayerId game fleet =
    let
        folder fleetCommand ( start, svgs ) =
            let
                ( end, newSvgs ) =
                    drawFleetCommand game start fleetCommand
            in
                ( end, List.append newSvgs svgs )

        start =
            List.head fleet.ships
                |> Maybe.map .currentPosition
                |> Maybe.withDefault (vector 0 0)

        ( end, svgs ) =
            List.foldl folder ( start, [] ) fleet.commands
    in
        svgs


drawFleetCommandQueues : G.Id -> Ui.UiShared a -> Game -> List Svg
drawFleetCommandQueues asViewedByPlayerId uiShared game =
    case uiShared.selection of
        Ui.FleetSelection ids ->
            G.selectedFleets ids game.fleets
                |> Dict.filter (\id fleet -> fleet.empireId == asViewedByPlayerId)
                |> Dict.values
                |> List.map (drawFleetCommandQueue asViewedByPlayerId game)
                |> List.concat

        _ ->
            []


star =
    Svg.circle
        [ A.cx "0"
        , A.cy "0"
        , A.r "0.02"
        , A.fill "#007"
        , A.stroke "#00f"
        , A.strokeWidth "0.002"
        ]
        []

planet orbitRadius =
    Svg.g
        []
        -- orbit
        [ Svg.circle
            [ A.cx "0"
            , A.cy "0"
            , A.r <| toString orbitRadius
            , A.fill "none"
            , A.stroke "#00f"
            , A.strokeWidth "0.002"
            ]
            []
        -- planet
        , Svg.circle
            [ A.cx <| toString orbitRadius
            , A.cy "0"
            , A.r "0.005"
            , A.fill "#070"
            , A.stroke "#0f0"
            , A.strokeWidth "0.002"
            ]
            []
        -- marker
        , Svg.circle
            [ A.cx <| toString orbitRadius
            , A.cy "0"
            , A.r "0.03"
            , A.fill "none"
            , A.stroke "#0f0"
            , A.strokeWidth "0.003"
            ]
            []
        ]


outerWellMarker =
    Svg.circle
        [ A.cx "0"
        , A.cy "0"
        , A.r <| toString starSystemOuterRadius
        , A.fill "none"
        , A.stroke "#007"
        , A.strokeWidth "0.006"
--         , A.strokeDasharray "6%, 6%"
        ]
        []


selectionBox model =
    case model.selectionBox of
        Nothing ->
            []

        Just startPosition ->
            let
                ( x, y, x', y' ) =
                    normalizeBox startPosition model.mousePosition

                w =
                    x' - x

                h =
                    y' - y

                rect =
                    Svg.rect
                        [ A.x <| toString x
                        , A.y <| toString y
                        , A.width <| toString w
                        , A.height <| toString h
                        , A.fill "#090"
                        , A.fillOpacity "0.2"
                        , A.stroke "#0d0"
                        , A.strokeWidth "0.005"
                        ]
                        []
            in
                [ rect ]


shipView : Bool -> Bool -> G.Id -> G.Ship -> Svg.Svg Msg
shipView isFriendly isSelected fleetId ship =
    let
        size =
            0.03
    in
        Svg.g
            [ A.transform <|
                String.join " " <|
                    [ "translate(" ++ G.vectorToString ship.currentPosition ++ ")"
                    , "scale(" ++ toString size ++ ")"
                    ]
            , onEventCooked "mousedown" MouseMove
            , onEventCooked "mouseup" (UserClicksFleet fleetId)
            ]
            [ FleetView.shipSvg isFriendly isSelected ship ]


fleetView : Bool -> Bool -> G.Fleet -> List (Svg.Svg Msg)
fleetView isFriendly isSelected fleet =
    List.map (shipView isFriendly isSelected fleet.id) fleet.ships


drawFleets : G.Id -> Game -> Ui.UiShared a -> List Svg
drawFleets asViewedByPlayerId game uiShared =
    let
        selectedIds =
            case uiShared.selection of
                Ui.FleetSelection ids ->
                    ids

                _ ->
                    Set.empty

        isFriendly fleet =
            fleet.empireId == asViewedByPlayerId

        displayFleet fleet =
            fleetView (isFriendly fleet) (Set.member fleet.id selectedIds) fleet
    in
        -- TODO display only fleets per asViewedByPlayerId
        Dict.values game.fleets
            |> List.map displayFleet
            |> List.concat


view : Int -> Game -> Ui.UiShared a -> Model -> Svg.Svg Msg
view asViewedByPlayerId game uiShared model =
    Svg.svg
        [ A.height "99vh"
        , A.viewBox "-1 -1 2 2"
        , A.preserveAspectRatio "xMidYMid meet"
        , A.id starSystemSvgId
        , onEventCooked "mousemove" MouseMove
        , onEventCooked "contextmenu" MouseMove
        , onEventCooked "mousedown" MousePress
        , onEventCooked "mouseup" MouseRelease
        ]
    <|
        List.concat <|
            [ [ star ]
            , [ planet <| starSystemOuterRadius / 3 ]
            , [ outerWellMarker ]
            , selectionBox model
            , (drawFleetCommandQueues asViewedByPlayerId uiShared game)
            , (drawFleets asViewedByPlayerId game uiShared)
            ]
