module View.Scoreboard exposing (..)

import Components
import Game exposing (Game)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, style)


textColor color =
    style [ ( "color", color ) ]


scoreView shipNameByOwnerId ( id, player, colorPattern ) =
    li
        []
        [ p
            [ class "score"
            , textColor colorPattern.bright
            ]
            [ text <| toString player.score ]
        , p
            [ class "name"
            , textColor colorPattern.bright
            ]
            [ shipNameByOwnerId id |> text ]
        ]


scoreboard : Game -> Html msg
scoreboard game =
    let
        shipsByOwnerId =
            Game.shipsByOwnerId game

        shipNameByOwnerId playerId =
            Dict.get playerId shipsByOwnerId
                |> Maybe.map (Tuple.second >> .name)
                |> Maybe.withDefault ""

        allScores =
            Components.all2 game ( .cPlayer, .cColorPattern )
                |> List.filter (\( id, player, color ) -> player.inputState /= Nothing)
                |> List.map (scoreView shipNameByOwnerId)
    in
        div
            [ class "scoreboard-container" ]
            [ ul
                [ class "scoreboard" ]
                allScores
            ]
