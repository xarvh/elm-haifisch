module View exposing (render)



import GameMain as Game
import GameCommon exposing (starSystemOuterRadius, vectorToString, normalizeBox)
import Html
import String
import Svg
import Svg.Events as E
import Svg.Attributes as A
import UI
import Math.Vector2 as V






drawShip viewerPlayerId ui ship =
    let
        size =
            0.05

        isSelected =
            List.member ship.id ui.selectedIds
    in
        Svg.path
            [   A.transform <| String.join " " <|
                    [ "translate" ++ vectorToString ship.position
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

            , UI.onLeftClickCooked (UI.UserClicksShip ship.id)
            ]
            []


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
        , UI.onMouseMove UI.MouseMove
        , UI.onLeftDownCooked UI.UserStartsSelectionBox
        , UI.onLeftClickCooked UI.UserLeftClicks
        , UI.onRightClickCooked UI.UserIssuesCommand
        ]
        <| List.concat
            [ [star]
            , [outerWellMarker]
            , selectionBox ui
            , List.map (drawShip viewerPlayerId ui) game.ships
            ]
