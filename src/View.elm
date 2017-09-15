module View exposing (..)

import Array exposing (Array)
import ColorPattern exposing (ColorPattern)
import Common exposing (..)
import Components
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Game exposing (Game)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Random
import Ship
import String
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time


worldRadius =
    Common.worldRadius



-- Splash


splash : String -> List (Html msg) -> Html msg
splash title content =
    H.div
        [ HA.class "splash-container full-window" ]
        [ H.div
            [ HA.class "splash" ]
            [ H.h2
                []
                [ H.text title ]
            , H.div
                [ HA.class "splash-content" ]
                content
            ]
        ]



-- Decoration


star =
    S.circle
        [ SA.cx "0"
        , SA.cy "0"
        , SA.r <| toString <| 0.02 * worldRadius
        , SA.fill "#eee"
        , SA.stroke "#aaa"
        , SA.strokeWidth <| toString <| 0.002 * worldRadius
        ]
        []


planet : Planet -> Svg a
planet p =
    let
        x =
            p.orbitRadius * cos p.angle

        y =
            p.orbitRadius * -1 * sin p.angle

        transform =
            "translate(" ++ toString x ++ ", " ++ toString y ++ ")"

        orbit =
            S.circle
                [ SA.cx "0"
                , SA.cy "0"
                , SA.r <| toString p.orbitRadius
                , SA.fill "none"
                , SA.stroke "#ddd"
                , SA.strokeWidth <| toString <| 0.002 * worldRadius
                ]
                []

        planet =
            S.circle
                [ SA.cx "0"
                , SA.cy "0"
                , SA.r <| toString <| p.surfaceRadius
                , SA.fill "#777"
                , SA.stroke "#ddd"
                , SA.strokeWidth <| toString <| 0.002 * worldRadius
                ]
                []

        satellites =
            (List.map satellite p.satellites)
    in
        S.g
            []
            [ orbit
            , S.g
                [ SA.transform transform ]
                (planet :: satellites)
            ]


satellite : Satellite -> Svg a
satellite s =
    S.g
        []
        -- orbit
        [ S.circle
            [ SA.cx "0"
            , SA.cy "0"
            , SA.r <| toString <| s.orbitRadius
            , SA.fill "none"
            , SA.stroke "#ddd"
            , SA.strokeWidth <| toString <| 0.002 * worldRadius
            ]
            []

        -- surface
        , S.circle
            [ SA.cx <| toString <| s.orbitRadius
            , SA.cy "0"
            , SA.r <| toString <| 0.003 * worldRadius
            , SA.fill "#777"
            , SA.stroke "#ddd"
            , SA.strokeWidth <| toString <| 0.002 * worldRadius
            , SA.transform <| "rotate(" ++ toString (s.angle / degrees 1) ++ ")"
            ]
            []
        ]


outerWellMarker =
    S.circle
        [ SA.cx "0"
        , SA.cy "0"
        , SA.r <| toString worldRadius
        , SA.fill "none"
        , SA.stroke "#aaa"
        , SA.strokeWidth <| toString <| 0.006 * worldRadius
        ]
        []



-- Ships


shipStrokWidth =
    0.005 * worldRadius


shipMesh : Float -> Vec2 -> Float -> ColorPattern -> Svg msg
shipMesh opacity position heading colorPattern =
    let
        tr polygon =
            List.map (rotateVector heading >> Vec2.add position) polygon

        vertices =
            tr Ship.mesh
                |> List.map vectorToString

        path =
            "M " ++ (String.join " L " vertices) ++ " Z"
    in
        S.path
            [ SA.d path
            , SA.style <|
                String.join "; " <|
                    [ "fill: " ++ colorPattern.dark
                    , "stroke: " ++ colorPattern.bright
                    , "stroke-width: " ++ toString shipStrokWidth
                    , "opacity: " ++ toString opacity
                    ]
            ]
            []


shipView : ( Components.EntityId, Game.ShipComponent, Vec2, Float, ColorPattern ) -> Svg msg
shipView ( id, ship, position, heading, colorPattern ) =
    case ship.status of
        Game.ShipActive ->
            shipMesh 1.0 position heading colorPattern

        Game.ShipSpawning ->
            let
                blinkPeriod =
                    0.25 * Time.second

                phase =
                    ship.respawnTime / blinkPeriod

                normalPhase =
                    phase - (toFloat <| floor phase)

                angularPhase =
                    normalPhase * turns 1

                opacity =
                    (1 + sin angularPhase) / 2
            in
                shipMesh opacity position heading colorPattern

        Game.ShipExploding ->
            let
                t =
                    ship.explodeTime / Ship.explosionDuration

                -- particle count
                n =
                    5

                -- max explosion size
                r =
                    0.1 * worldRadius

                particleByIndex index =
                    let
                        a =
                            turns (index / n)

                        x =
                            t * r * cos a

                        y =
                            t * r * sin a
                    in
                        S.circle
                            [ SA.cx <| toString x
                            , SA.cy <| toString y
                            , SA.r <| toString <| (t * 0.9 + 0.1) * 0.2 * worldRadius
                            , SA.opacity <| toString <| (1 - t) / 3
                            , SA.fill colorPattern.dark
                            , SA.stroke colorPattern.bright
                            , SA.strokeWidth <| toString <| shipStrokWidth * 2
                            ]
                            []
            in
                S.g
                    [ SA.transform <| "translate(" ++ vectorToString position ++ ")"
                    ]
                    (List.map particleByIndex <| List.map toFloat <| List.range 0 <| n - 1)



-- Projectiles
{-
   projectile : PlayersById -> Projectile -> Svg msg
   projectile playersById projectile =
       let
           colorPattern =
               Dict.get projectile.playerId playersById
                   |> Maybe.map .colorPattern
                   |> Maybe.withDefault ColorPattern.neutral

           size =
               0.01 * worldRadius

           ( x, y ) =
               Vec2.toTuple projectile.position
       in
           S.g
               []
               [ S.circle
                   [ SA.cx <| toString <| x
                   , SA.cy <| toString <| y
                   , SA.r <| toString <| 0.01 * worldRadius
                   , SA.fill colorPattern.bright
                   , SA.stroke colorPattern.dark
                   , SA.strokeWidth <| toString <| 0.001 * worldRadius
                   ]
                   []
               ]
-}
-- Main


viewbox worldSize =
    let
        ( w, h ) =
            Vec2.toTuple worldSize
    in
        String.join " " <| List.map toString [ -w / 2, -h / 2, w, h ]


game : Vec2 -> Game -> Html a
game worldSize game =
    let
        ships =
            Components.all4 game ( .cShip, .cPosition, .cHeading, .cColorPattern )

        entities =
            [ [ star, outerWellMarker ]
            , List.map shipView ships
            ]

        {-

           entities =
               , List.map planet model.planets
               , List.map (projectile playersById) model.projectiles
               ]
        -}
    in
        H.div
            [ HA.class "star-system-container full-window" ]
            [ S.svg
                [ SA.viewBox (viewbox worldSize)
                ]
                [ S.g
                    [ SA.transform "scale(1, -1)" ]
                    (List.concat entities)
                ]
            ]
