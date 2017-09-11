module View exposing (..)

import Array exposing (Array)
import Common exposing (..)
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Events as HE
import Html.Attributes as HA
import Game
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Random
import Ship
import String
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time


-- TODO: this module is a mess, needs some reorganisation
-- TODO: scale most things by worldRadius?


worldRadius =
    Game.worldRadius


projectileRadius =
    0.01 * worldRadius


shipStrokWidth =
    0.005 * worldRadius



-- Player coloration


colorations : Array Coloration
colorations =
    Array.fromList
        -- bright, dark
        [ ( "#f00", "#900", "red" )
        , ( "#0f0", "#090", "green" )
        , ( "#00f", "#009", "blue" )
        , ( "#0ee", "#0bb", "cyan" )
        , ( "#f0f", "#909", "purple" )
        , ( "#ee0", "#bb0", "yellow" )
        ]


getColoration playersById id =
    Dict.get id playersById
        |> Maybe.map .coloration
        |> Maybe.withDefault ( "", "", "" )



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


background =
    let
        ( fill, stroke, strokeWidth ) =
            ( "#ffffff", "#f7f7f7", "4px" )
    in
        S.svg
            [ SA.class "ui-background full-window"
            ]
            [ S.defs
                []
                [ S.pattern
                    [ SA.id "hex-background"
                    , SA.width "56px"
                    , SA.height "100px"
                    , SA.patternUnits "userSpaceOnUse"
                    , SA.patternTransform "scale(0.5)"
                    ]
                    [ S.rect
                        [ SA.width "100%"
                        , SA.height "100%"
                        , SA.fill fill
                        ]
                        []
                    , S.path
                        [ SA.d "M28 66L0 50L0 16L28 0L56 16L56 50L28 66L28 100"
                        , SA.fill fill
                        , SA.stroke stroke
                        , SA.strokeWidth strokeWidth
                        ]
                        []
                    ]
                ]
            , S.rect
                [ SA.fill "url(#hex-background)"
                , SA.width "100%"
                , SA.height "100%"
                ]
                []
            ]


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



-- SHIPS
-- shipMesh : Float -> Int -> Ship -> Svg a


shipMesh opacity ( bright, dark, _ ) ship =
    let
        vertices =
            Ship.transform ship Ship.mesh
                |> List.map vectorToString

        path =
            "M " ++ (String.join " L " vertices) ++ " Z"
    in
        S.path
            [ SA.d path
            , SA.style <|
                String.join "; " <|
                    [ "fill: " ++ dark
                    , "stroke: " ++ bright
                    , "stroke-width: " ++ toString shipStrokWidth
                    , "opacity: " ++ toString opacity
                    ]
            ]
            []



-- ship : Int -> Ship -> Svg a


ship playersById ship =
    let
        coloration =
            getColoration playersById ship.playerId
    in
        case ship.status of
            Active ->
                shipMesh 1.0 coloration ship

            Spawning ->
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
                    shipMesh opacity coloration ship

            Exploding ->
                let
                    t =
                        ship.explodeTime / Ship.explosionDuration

                    -- particle count
                    n =
                        5

                    -- max explosion size
                    r =
                        0.1 * worldRadius

                    ( bright, dark, _ ) =
                        coloration

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
                                , SA.fill dark
                                , SA.stroke bright
                                , SA.strokeWidth <| toString <| shipStrokWidth * 2
                                ]
                                []
                in
                    S.g
                        [ SA.transform <| "translate(" ++ vectorToString ship.position ++ ")"
                        ]
                        (List.map particleByIndex <| List.map toFloat <| List.range 0 <| n - 1)



-- Projectiles


projectile playersById p =
    let
        ( bright, dark, _ ) =
            getColoration playersById p.playerId

        size =
            0.01 * worldRadius

        ( x, y ) =
            Vec2.toTuple p.position
    in
        S.g
            {- TODO: use linear transforms instead? Run some benchmarks!
               [ SA.transform <|
                   String.join " " <|
                       [ "translate(" ++ vectorToString p.position ++ ")"
                       , "scale(" ++ toString size ++ ")"
                       ]
               ]
            -}
            []
            [ S.circle
                [ SA.cx <| toString <| x
                , SA.cy <| toString <| y
                , SA.r <| toString <| 0.01 * worldRadius
                , SA.fill bright
                , SA.stroke dark
                , SA.strokeWidth <| toString <| 0.001 * worldRadius
                ]
                []
            ]


score remapMsg shipsById player =
    let
        ( bright, dark, _ ) =
            player.coloration

        name =
            case Dict.get player.controllerId shipsById of
                Just ship ->
                    ship.name

                Nothing ->
                    "-"

        color c =
            HA.style [ ( "color", c ) ]
    in
        H.li
            []
            [ H.p
                [ HA.class "name", color bright ]
                [ H.text name ]
            , H.p
                [ HA.class "score", color bright ]
                [ H.text <| toString player.score ]
            , H.button
                [ HE.onClick (remapMsg player.controllerId) ]
                [ H.text "Remap" ]
            ]


scoreboard remapMsg playersById shipsById =
    H.div
        [ HA.class "scoreboard-container" ]
        [ H.ul
            [ HA.class "scoreboard" ]
            (List.map (score remapMsg shipsById) (Dict.values playersById |> List.filter .isConnected))
        ]


viewbox worldSize =
    let
        ( w, h ) =
            Vec2.toTuple worldSize
    in
        String.join " " <| List.map toString [ -w / 2, -h / 2, w, h ]


game : Vec2 -> Dict Int Player -> Game.Model -> Html a
game worldSize playersById model =
    let
        entities =
            [ [ star
              , outerWellMarker
              ]
            , List.map planet model.planets
            , List.map (ship playersById) (Dict.values model.shipsById)
            , List.map (projectile playersById) model.projectiles
            ]
    in
        H.div
            [ HA.class "star-system-container full-window" ]
            [ S.svg
                [ SA.viewBox (viewbox worldSize)
                ]
                [ S.g
                    [ SA.transform "scale(1, -1)"]
                    (List.concat entities)
                ]
            ]
