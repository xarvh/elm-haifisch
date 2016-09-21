module FleetView exposing (..)

import String
import Svg
import Svg.Attributes as A


shipSvg isFriendly isSelected ship =
    let
        stroke =
            if isFriendly then
                "#0f0"
            else
                "#f00"

        fill =
            if isSelected then
                stroke
            else if isFriendly then
                "#088"
            else
                "#900"

        strokeWidth =
            "0.15"
    in
        Svg.path
            [ A.transform <| "rotate(" ++ toString (ship.angle / degrees 1) ++ ")"
            , A.d """
            M -0.3,0
            L -0.5, -0.5
            L 1, 0
            L -0.5, +0.5
            Z
            """
            , A.style <|
                String.join "; " <|
                    [ "fill: " ++ fill
                    , "stroke: " ++ stroke
                    , "stroke-width: " ++ strokeWidth
                    ]
            ]
            []
