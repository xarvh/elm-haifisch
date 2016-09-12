module View exposing (render)


-- import GameMain
import GameCommon as G exposing
    ( Game
    , starSystemOuterRadius
    , vectorToString
    , normalizeBox
    , vector
    )

import Html
import Html.Attributes
import String
import Svg
import Svg.Events as E
import Svg.Attributes as A
import UI


drawShip isSelected fleetId ship =
    let
        size =
            0.05
    in
        Svg.path
            [   A.transform <| String.join " " <|
                    [ "translate(" ++ vectorToString ship.currentPosition ++ ")"
                    , "scale(" ++ toString size ++ ")"
                    , "rotate(" ++ toString (ship.angle / degrees 1) ++ ")"
                    ]

            ,   A.d """
                M -0.3,0
                L -0.5, -0.5
                L 1, 0
                L -0.5, +0.5
                Z
                """

            , A.style <| String.join "; " <|
                [ "fill: #088"
                , "stroke: " ++ if isSelected then "#0f0" else "#055"
                , "stroke-width: " ++ toString (if isSelected then size * 3 else size)
                ]

            , UI.onEventCooked "mousedown" UI.StarSystemMouseMove
            , UI.onEventCooked "mouseup" (UI.UserClicksFleet fleetId)
            ]
            []



drawFleet viewerPlayerId ui fleet =
    let
        isSelected =
            List.member fleet.id ui.selectedIds
    in
        List.map (drawShip isSelected fleet.id) fleet.ships





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


drawFleetCommandQueue viewerPlayerId ui fleet =
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





render : Int -> Game -> UI.Model -> Html.Html UI.Message
render viewerPlayerId game ui =
    Html.div
        [ Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "width", "100wh" )
            , ( "height", "100vh" )
            , ( "top", "50%" )
            , ( "left", "50%" )
            , ( "transform", "translateX(-50%) translateY(-50%)" )
            ]
        ]
        [ Svg.svg
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
                (List.map (drawFleet viewerPlayerId ui) game.fleets)
                ++
                (List.map (drawFleetCommandQueue viewerPlayerId ui) game.fleets)
        ]
