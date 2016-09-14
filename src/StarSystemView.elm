module StarSystemView exposing (..)


import GameCommon as G exposing
    ( Game
    , starSystemOuterRadius
    , vectorToString
    , normalizeBox
    , vector
    )

import Svg
import Svg.Attributes as A
import UI
import FleetView




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


drawFleetCommandQueue asViewedByPlayerId ui fleet =
    if not <| List.member fleet.id ui.selectedIds
    then []
    else
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



selectionBox ui =
    case ui.selectionBox of
        Nothing -> []
        Just startPosition ->
            let
                (x, y, x', y') = normalizeBox startPosition ui.starSystemMousePosition
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



drawFleets asViewedByPlayerId game ui =
    let
        mapper fleet =
            let
                isSelected =
                    List.member fleet.id ui.selectedIds
            in
                FleetView.view isSelected fleet

    in
        -- TODO display only fleets per asViewedByPlayerId
        List.map mapper game.fleets



view : Int -> Game -> UI.Model -> Svg.Svg UI.Msg
view asViewedByPlayerId game ui =
    Svg.svg
        [ A.height "100vh"
        , A.viewBox "-1 -1 2 2"
        , A.preserveAspectRatio "xMidYMid meet"

        , A.style "border: 1px solid #333333;"

        , A.id UI.starSystemSvgId
        , UI.onEventCooked "mousemove" UI.StarSystemMouseMove
        , UI.onEventCooked "contextmenu" UI.StarSystemMouseMove
        , UI.onEventCooked "mousedown" UI.StarSystemMousePress
        , UI.onEventCooked "mouseup" UI.StarSystemMouseRelease
        ]
        <| List.concat <|
            [ [star]
            , [outerWellMarker]
            , selectionBox ui
            ]
            ++
            (drawFleets asViewedByPlayerId game ui)
            ++
            (List.map (drawFleetCommandQueue asViewedByPlayerId ui) game.fleets)
