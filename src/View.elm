module View exposing (render)



import GameMain as Game
import GameCommon exposing (starSystemOuterRadius)
import Html
import String
import Svg
import Svg.Events as E
import Svg.Attributes as A
import UI



drawShip viewerPlayerId ship =
    let
        size = 0.05
    in
        Svg.path
            [   A.transform <| String.join " " <|
                    [ "translate(" ++ toString ship.position.x ++ "," ++ toString ship.position.y ++ ")"
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
                , "stroke: #055"
                , "stroke-width: " ++ toString size
                ]

            , E.onClick (UI.PlayerInput Game.ShipMove)
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






render : Int -> Game.Game -> Html.Html UI.Message
render viewerPlayerId gameModel =
            let
                q = Debug.log "D" gameModel.ships
            in
    Svg.svg
        [ A.width "100%"
        , A.height "100%"
        , A.viewBox "-1 -1 2 2"
        , A.preserveAspectRatio "xMidYMid meet"

        , A.style "border: 1px solid #333333;"


        ]
        <| List.concat
            [ [star]
            , [outerWellMarker]
            , List.map (drawShip viewerPlayerId) gameModel.ships
            ]
