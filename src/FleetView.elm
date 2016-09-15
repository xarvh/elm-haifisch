module FleetView exposing (..)


import GameCommon as G exposing ( Id, Fleet, Ship )
import String
import Svg
import Svg.Attributes as A
import UI



shipSvg isSelected ship =
    Svg.path
        [   A.transform <| "rotate(" ++ toString (ship.angle / degrees 1) ++ ")"

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
            , "stroke-width: " ++ toString (if isSelected then 0.15 else 0.05)
            ]
        ]
        []



shipView : Bool -> Id -> Ship -> Svg.Svg UI.Msg
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

            , UI.onEventCooked "mousedown" UI.StarSystemMouseMove
            , UI.onEventCooked "mouseup" (UI.UserClicksFleet fleetId)
            ]
            [ shipSvg isSelected ship ]


view : Bool -> Fleet -> List (Svg.Svg UI.Msg)
view isSelected fleet =
    List.map (shipView isSelected fleet.id) fleet.ships
