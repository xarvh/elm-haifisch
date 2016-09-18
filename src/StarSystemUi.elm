module StarSystemUi exposing (..)


import GameCommon as G exposing
    ( Game
    , starSystemOuterRadius
    , vectorToString, normalizeBox
    , Vector, vector
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




update : Msg -> Game -> Ui.UiShared a -> Model -> (Model, Ui.Selection, List G.Command)
update msg game uiShared model =

    case msg of

        MouseMove button pos ->
            ( { model | mousePosition = pos }, uiShared.selection, [] )

        MousePress button pos ->
            manageMouse button Ui.MousePress game uiShared model

        MouseRelease button pos ->
            manageMouse button Ui.MouseRelease game uiShared model

        UserClicksFleet fleetId button pos ->
            select (Ui.FleetSelection <| Set.singleton fleetId) model




select selection model =
    ( { model | selectionBox = Nothing }, selection, [] )









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
                                    Nothing -> Ui.NoSelection
                                    Just startPosition -> boxSelection startPosition model.mousePosition game
                        in
                            select selection model


boxSelection start end game =
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

        fleets =
            List.filterMap fm game.fleets

        selection =
            if List.isEmpty fleets
            then Ui.NoSelection
            else Ui.FleetSelection <| Set.fromList fleets
    in
        selection




command game uiShared model =
    let
        commands =
            case uiShared.selection of
                Ui.NoSelection -> []
                Ui.FleetSelection fleetIds ->
                    [G.FleetCommand (Set.toList fleetIds) (Ui.queueMode uiShared) (G.ThrustTo model.mousePosition)]
    in
        ( model, uiShared.selection, commands )




-------------------------------------------------
-- TODO: once StarSystemUi becomes full-screen coordinate transformation can be simplified


starSystemSvgId =
    "starsystem-view"


decodeStarSystemMouse tagger =
    let
        toVector (x, y) =
            vector x y

        decodeMouseButtons which button =
            if which == 1 || button == 0
            then Ui.MouseLeft
            else if which == 3 || button == 2
                then Ui.MouseRight
                else Ui.MouseMid

        mapper clientX clientY which button =
            tagger (decodeMouseButtons which button) <| toVector <| SvgMouse.transform ("svg#" ++ starSystemSvgId) clientX clientY
    in
        Json.object4 mapper ("clientX" := Json.int) ("clientY" := Json.int) ("which" := Json.int) ("button" := Json.int)


onEventCooked eventName tagger =
    Html.Events.onWithOptions eventName { stopPropagation = True, preventDefault = True } <| decodeStarSystemMouse tagger


----------------






drawFleetCommand : G.Vector -> G.FleetCommand -> (G.Vector, Svg.Svg a)
drawFleetCommand start shipCommand =
    case shipCommand of
        G.ThrustTo end ->
            ( end
            , Svg.polyline
                [ A.points <| vectorToString start ++ " " ++ vectorToString end
                , A.fill "none"
                , A.stroke "#0d0"
                , A.strokeWidth "0.003"
                ]
                []
            )

        -- TODO: add MergeWith
        _ ->
            ( start, Svg.polyline [] [] )


drawFleetCommandQueue asViewedByPlayerId fleet =
    let
        folder fleetCommand (start, svgs) =
            let (end, svg) = drawFleetCommand start fleetCommand
            in (end, svg :: svgs)

        start =
            List.head fleet.ships
            |> Maybe.map .currentPosition
            |> Maybe.withDefault (vector 0 0)

        (end, svgs) =
            List.foldl folder (start, []) fleet.commands
    in
        svgs


drawFleetCommandQueues asViewedByPlayerId uiShared fleets =
    case uiShared.selection of
        Ui.FleetSelection ids ->
            G.selectedFleets ids fleets
            |> List.map (drawFleetCommandQueue asViewedByPlayerId)
        _ -> []




star =
    Svg.circle
        [ A.cx "0"
        , A.cy "0"
        , A.r "0.05"
        , A.fill "#ff0"
        ]
        []



outerWellMarker =
    Svg.ellipse
        [ A.cx "0"
        , A.cy "0"
        , A.rx <| toString starSystemOuterRadius
        , A.ry <| toString starSystemOuterRadius

        , A.fill "none"
        , A.stroke "#999"
        , A.strokeWidth "0.01"
        , A.strokeDasharray "6%, 6%"
        ]
        []



selectionBox model =
    case model.selectionBox of
        Nothing -> []
        Just startPosition ->
            let
                (x, y, x', y') = normalizeBox startPosition model.mousePosition
                w = x' - x
                h = y' - y

                rect = Svg.rect
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







shipView : Bool -> G.Id -> G.Ship -> Svg.Svg Msg
shipView isSelected fleetId ship =
    let
        size =
            0.05
    in
        Svg.g
            [   A.transform <| String.join " " <|
                    [ "translate(" ++ G.vectorToString ship.currentPosition ++ ")"
                    , "scale(" ++ toString size ++ ")"
                    ]

            , onEventCooked "mousedown" MouseMove
            , onEventCooked "mouseup" (UserClicksFleet fleetId)
            ]
            [ FleetView.shipSvg isSelected ship ]


fleetView : Bool -> G.Fleet -> List (Svg.Svg Msg)
fleetView isSelected fleet =
    List.map (shipView isSelected fleet.id) fleet.ships













drawFleets asViewedByPlayerId game uiShared =
    let
        selectedIds =
            case uiShared.selection of
                Ui.FleetSelection ids -> ids
                _ -> Set.empty

        displayFleet fleet =
            fleetView (Set.member fleet.id selectedIds) fleet

    in
        -- TODO display only fleets per asViewedByPlayerId
        List.map displayFleet game.fleets






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
        <| List.concat <|
            [ [star]
            , [outerWellMarker]
            , selectionBox model
            ]
            ++
            (drawFleets asViewedByPlayerId game uiShared)
            ++
            drawFleetCommandQueues asViewedByPlayerId uiShared game.fleets
