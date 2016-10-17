module View exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Game exposing (Ship, Projectile, Vector, vectorToString)
import Random
import String
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Time
import Math.Vector2 as V


-- TODO: this module is a mess, needs some reorganisation
-- TODO: use Svg.Lazy?


worldRadius =
    Game.worldRadius


projectileRadius =
    0.01 * worldRadius


shipStrokWidth =
    0.005 * worldRadius



-- Player coloration


type alias Coloration =
    ( String, String )


colorations : Array Coloration
colorations =
    Array.fromList
        -- bright, dark
        [ ( "#f00", "#900" )
        , ( "#0f0", "#090" )
        , ( "#00f", "#009" )
        , ( "#0ee", "#0bb" )
        , ( "#f0f", "#909" )
        , ( "#ee0", "#bb0" )
        ]


getColoration playersById id =
    Dict.get id playersById
        |> Maybe.map .coloration
        |> Maybe.withDefault ( "", "" )



-- Splash


splash hasControllers =
    if hasControllers then
        H.text ""
    else
        H.div
            [ HA.class "splash-container full-window" ]
            [ H.div
                [ HA.class "splash" ]
                [ H.h1
                    []
                    [ H.text "Haifisch" ]
                , H.p
                    []
                    [ H.text "No gamepads detected, you need at least TWO to play." ]
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
        , SA.r "0.02"
        , SA.fill "#666"
        , SA.stroke "#ddd"
        , SA.strokeWidth "0.002"
        ]
        []


planet orbitRadius =
    S.g
        []
        -- orbit
        [ S.circle
            [ SA.cx "0"
            , SA.cy "0"
            , SA.r <| toString orbitRadius
            , SA.fill "none"
            , SA.stroke "#ddd"
            , SA.strokeWidth "0.002"
            ]
            []
          -- planet
        , S.circle
            [ SA.cx <| toString orbitRadius
            , SA.cy "0"
            , SA.r "0.005"
            , SA.fill "#777"
            , SA.stroke "#ddd"
            , SA.strokeWidth "0.002"
            ]
            []
          -- marker
        , S.circle
            [ SA.cx <| toString orbitRadius
            , SA.cy "0"
            , SA.r "0.03"
            , SA.fill "none"
            , SA.stroke "#ddd"
            , SA.strokeWidth "0.003"
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
        , SA.strokeWidth "0.006"
        ]
        []



-- SHIPS


-- shipMesh : Float -> Int -> Ship -> Svg a
shipMesh opacity (bright, dark) ship =
    let
        vertices =
            Game.shipTransform ship Game.shipMesh
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
            getColoration playersById ship.controllerId
    in
        case ship.status of
            Game.Active ->
                shipMesh 1.0 coloration ship

            Game.Spawning ->
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

            Game.Exploding ->
                let
                    t =
                        ship.explodeTime / Game.explosionDuration

                    -- particle count
                    n =
                        5

                    -- max explosion size
                    r =
                        0.1 * worldRadius

                    ( bright, dark ) =
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
                        (List.map particleByIndex [0..n - 1])



-- Projectiles


projectileSvg playersById p =
    let
        ( bright, dark ) =
            getColoration playersById p.ownerControllerId
    in
        S.circle
            [ SA.cx <| toString <| V.getX p.position
            , SA.cy <| toString <| V.getY p.position
            , SA.r "1"
            , SA.fill bright
            , SA.stroke dark
            , SA.strokeWidth "0.1"
            ]
            []


-- projectileView : Int -> Projectile -> Svg a
projectileView colorOffset p =
    let
        size =
            0.01
    in
        S.g
            [ SA.transform <|
                String.join " " <|
                    [ "translate(" ++ vectorToString p.position ++ ")"
                    , "scale(" ++ toString size ++ ")"
                    ]
            ]
            [ projectileSvg colorOffset p ]



{-
   selectionCross uiShared game =
       let
           fleets =
               Ui.selectedFleets uiShared game

           addShip ship ( xs, ys ) =
               ( V.getX ship.currentPosition :: xs, V.getY ship.currentPosition :: ys )

           addFleet id fleet accum =
               List.foldl addShip accum fleet.ships

           ( xs, ys ) =
               Dict.foldl addFleet ( [], [] ) fleets

           spacing =
               0.1

           minX =
               List.minimum xs |> Maybe.withDefault 0 |> (flip (-)) spacing |> max -1

           maxX =
               List.maximum xs |> Maybe.withDefault 0 |> (+) spacing |> min 1

           minY =
               List.minimum ys |> Maybe.withDefault 0 |> (flip (-)) spacing |> max -1

           maxY =
               List.maximum ys |> Maybe.withDefault 0 |> (+) spacing |> min 1

           midX =
               (maxX + minX) / 2

           midY =
               (maxY + minY) / 2

           line x1 y1 x2 y2 =
               S.line
                   [ SA.x1 <| toString x1
                   , SA.y1 <| toString y1
                   , SA.x2 <| toString x2
                   , SA.y2 <| toString y2
                   , SA.stroke "#0f0"
                     --                 , SA.opacity "0.2"
                   , SA.strokeWidth "0.002"
                   ]
                   []
       in
           if Dict.isEmpty fleets then
               []
           else
               -- left and right horizontal lines
               [ line -1 midY minX midY
               , line maxX midY 1 midY
                 -- vertical lines
               , line midX -1 midX minY
               , line midX maxY midX 1
               ]
-}
---


score playersById ship =
    let
        ( bright, dark ) =
            getColoration playersById ship.controllerId

        score =
            case Dict.get ship.controllerId playersById of
                Just player -> player.score
                Nothing -> 0

        color c =
            HA.style [ ( "color", c ) ]
    in
        H.li
            []
            [ H.p [ HA.class "name", color bright ] [ H.text ship.name ]
            , H.p [ HA.class "score", color bright ] [ H.text <| toString score ]
            ]


scoreboard playersById shipsById =
    H.div
        [ HA.class "scoreboard-container" ]
        [ H.ul
            [ HA.class "scoreboard" ]
            (List.map (score playersById) (Dict.values shipsById))
        ]



---


viewbox worldSize =
    let
        ( w, h ) =
            V.toTuple worldSize
    in
        String.join " " <| List.map toString [ -w / 2, -h / 2, w, h ]


game : Vector -> Dict Int { score : Int, coloration : Coloration } -> Game.Model -> Html a
game worldSize playersById model =
    H.div
        [ HA.class "star-system-container full-window" ]
        [ S.svg
            [ SA.viewBox (viewbox worldSize)
            ]
          <|
            [ star
            , planet (worldRadius / 3)
            , outerWellMarker
            ]
                ++ (List.map (ship playersById) (Dict.values model.shipsById))
                ++ (List.map (projectileView playersById) model.projectiles)
        ]
