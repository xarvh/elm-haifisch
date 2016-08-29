module View exposing (render)


import GameMain as Game
import GameCommon as G exposing (starSystemOuterRadius, vectorToString, normalizeBox)
import Html
import String
import Svg
import Svg.Events as E
import Svg.Attributes as A
import UI


drawShip viewerPlayerId ui ship =
    let
        size =
            0.05

        isSelected =
            List.member ship.id ui.selectedIds
    in
        Svg.path
            [   A.transform <| String.join " " <|
                    [ "translate(" ++ vectorToString ship.position ++ ")"
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
            , UI.onEventCooked "mouseup" (UI.UserClicksShip ship.id)
            ]
            []


drawShipCommand : G.Vector -> G.ShipCommand -> (G.Vector, Svg.Svg a)
drawShipCommand start shipCommand =
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


drawShipCommandQueue viewerPlayerId ui ship =
    if not <| List.member ship.id ui.selectedIds
    then []
    else
        let
            folder shipCommand (start, svgs) =
                let (end, svg) = drawShipCommand start shipCommand
                in (end, svg :: svgs)

            (end, svgs) =
                List.foldl folder (ship.position, []) ship.commands
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





render : Int -> Game.Game -> UI.Model -> Html.Html UI.Message
render viewerPlayerId game ui =
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
            , List.map (drawShip viewerPlayerId ui) game.ships
            ]
            ++
            (List.map (drawShipCommandQueue viewerPlayerId ui) game.ships)
