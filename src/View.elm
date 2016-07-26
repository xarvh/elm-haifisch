module View exposing (render)



import Game.Main as Game
import Html
import String
import Svg
import Svg.Attributes as A



drawShip viewerPlayerId (id, player) =
    let
        ship = player.ship

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
            ]
            []




render : Int -> Game.Model -> Html.Html a
render viewerPlayerId gameModel =
    Svg.svg
        [ A.width "50%"
        , A.height "50%"
        , A.viewBox "0 0 1 1"
        ]
        <| List.concat
            [ List.map (drawShip viewerPlayerId) gameModel.players
            ]