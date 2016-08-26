module View exposing (render)



import GameMain as Game
import GameCommon exposing (starSystemOuterRadius, vectorToString)
import Html
import String
import Svg
import Svg.Events as E
import Svg.Attributes as A
import UI






drawShip viewerPlayerId ui ship =
    let
        size = 0.05
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
                , "stroke: " ++ if List.member ship.id ui.selectedIds then "#0ff" else "#055"
                , "stroke-width: " ++ toString size
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






render : Int -> Game.Game -> UI.Model -> Html.Html UI.Message
render viewerPlayerId game ui =
    Svg.svg
        [ A.height "100vh"
        , A.viewBox "-1 -1 2 2"
        , A.preserveAspectRatio "xMidYMid meet"

        , A.style "border: 1px solid #333333;"

        , A.id UI.starSystemSvgId
        , UI.onMouseMove UI.MouseMove
        , UI.onLeftClickCooked UI.UserSelectsNone

        , UI.onRightClickCooked UI.UserIssuesCommand
        ]
        <| List.concat
            [ [star]
            , [outerWellMarker]
            , List.map (drawShip viewerPlayerId ui) game.ships
            ]
