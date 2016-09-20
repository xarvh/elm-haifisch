module FleetView exposing (..)

import String
import Svg
import Svg.Attributes as A


shipSvg isFriendly isSelected ship =
    let
        fill =
            if isFriendly then
                "#088"
            else
                "#900"

        strokeWidth =
            if isSelected then
                "0.15"
            else
                "0.05"

        stroke =
            case ( isSelected, isFriendly ) of
                ( False, False ) ->
                    "#550"

                ( False, True ) ->
                    "#055"

                ( True, False ) ->
                    "#f00"

                ( True, True ) ->
                    "#0f0"
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
