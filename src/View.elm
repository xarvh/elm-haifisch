module View exposing (..)

import Array
import Dict
import Html as H exposing (Html)
import Html.Attributes as HA
import Game exposing (Ship, Projectile, Vector, vectorToString)
import String
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Math.Vector2 as V
import Window


-- TODO: this module is a mess, needs some reorganisation
-- TODO: use Svg.Lazy?


worldRadius =
    Game.worldRadius


projectileRadius =
    0.01



-- Player colors


playerColorsArray =
    Array.fromList
        -- stroke, fill
        [ ( "#f00", "#900" )
        , ( "#0f0", "#088" )
        ]


playerColor : Int -> Int -> ( String, String )
playerColor colorOffset controllerId =
    let
        index =
            (colorOffset + controllerId) % Array.length playerColorsArray

        q = Debug.log "" (index, colorOffset, controllerId, Array.length playerColorsArray)
    in
        Array.get index playerColorsArray
            |> Maybe.withDefault ( "", "" )



-- Splash


splash hasControllers =
    if hasControllers then
        H.text ""
    else
        H.div
            [ HA.class "splash" ]
            [ H.h1
                []
                [ H.text "Haifisch" ]
            , H.text
                "No gamepads detected, you need at least TWO to play."
            ]



-- Decoration


background =
    let
        ( fill, stroke, strokeWidth ) =
            if True then
                ( "#000030", "#000075", "3px" )
            else
                ( "#000030", "black", "10px" )
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
        , SA.fill "#007"
        , SA.stroke "#00f"
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
            , SA.stroke "#00f"
            , SA.strokeWidth "0.002"
            ]
            []
          -- planet
        , S.circle
            [ SA.cx <| toString orbitRadius
            , SA.cy "0"
            , SA.r "0.005"
            , SA.fill "#070"
            , SA.stroke "#0f0"
            , SA.strokeWidth "0.002"
            ]
            []
          -- marker
        , S.circle
            [ SA.cx <| toString orbitRadius
            , SA.cy "0"
            , SA.r "0.03"
            , SA.fill "none"
            , SA.stroke "#0f0"
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
        , SA.stroke "#007"
        , SA.strokeWidth "0.006"
        ]
        []



-- SHIPS


shipSvg colorOffset ship =
    let
        ( stroke, fill ) =
            playerColor colorOffset ship.controllerId

        strokeWidth =
            "0.15"

        --         ( hx, hy ) =
        --             V.toTuple ship.heading
        --
        --         heading =
        --             atan2 hy hx
    in
        S.path
            [ SA.transform <| "rotate(" ++ toString (ship.heading / degrees 1) ++ ")"
            , SA.d """
            M -0.3,0
            L -0.5, -0.5
            L 1, 0
            L -0.5, +0.5
            Z
            """
            , SA.style <|
                String.join "; " <|
                    [ "fill: " ++ fill
                    , "stroke: " ++ stroke
                    , "stroke-width: " ++ strokeWidth
                    ]
            ]
            []


shipView : Int -> Ship -> Svg Never
shipView colorOffset ship =
    let
        size =
            0.03
    in
        S.g
            [ SA.transform <|
                String.join " " <|
                    [ "translate(" ++ vectorToString ship.position ++ ")"
                    , "scale(" ++ toString size ++ ")"
                    ]
            ]
            [ shipSvg colorOffset ship ]



-- Projectiles


projectileSvg p =
    S.circle
        [ SA.cx <| toString <| V.getX p.position
        , SA.cy <| toString <| V.getY p.position
        , SA.r "0.5"
        , SA.fill "#0a0"
        , SA.stroke "#0f0"
        , SA.strokeWidth "0.1"
        ]
        []


projectileView : Int -> Projectile -> Svg Never
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
            [ projectileSvg p ]



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


viewbox worldSize =
    let
        ( w, h ) =
            V.toTuple worldSize
    in
        String.join " " <| List.map toString [ -w / 2, -h / 2, w, h ]


view : Vector -> Game.Model -> Svg Never
view worldSize model =
    S.svg
        [ SA.viewBox (viewbox worldSize)
        ]
    <|
        [ star
        , planet (worldRadius / 3)
        , outerWellMarker
        ]
            ++ (List.map (shipView model.colorOffset) (Dict.values model.shipsById))
            ++ (List.map (projectileView model.colorOffset) model.projectiles)


game : Vector -> Game.Model -> Html Never
game worldSize model =
    H.div
        [ HA.class "star-system-container full-window" ]
        [ view worldSize model
        ]
