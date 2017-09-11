module View.Scoreboard exposing (..)

import Common exposing (Player, Ship, Id)
import Dict exposing (Dict)
import Dict.Extra
import Html exposing (..)
import Html.Attributes exposing (class, style)


textColor color =
    style [ ( "color", color ) ]


shipName : Player -> Dict Id Ship -> String
shipName player shipsById =
    shipsById
        |> Dict.Extra.find (\shipId ship -> ship.playerId == player.id)
        |> Maybe.map (Tuple.second >> .name)
        |> Maybe.withDefault ""


score shipsById player =
    li
        []
        [ p
            [ class "score"
            , textColor player.colorPattern.bright
            ]
            [ text <| toString player.score ]
        , p
            [ class "name"
            , textColor player.colorPattern.bright
            ]
            [ shipName player shipsById |> text ]
        ]


scoreboard : List Player -> Dict Id Ship -> Html msg
scoreboard players shipsById =
    let
        visiblePlayers =
            players
                |> List.filter .isConnected
                |> List.sortBy .id
    in
        div
            [ class "scoreboard-container" ]
            [ ul
                [ class "scoreboard" ]
                (List.map (score shipsById) visiblePlayers)
            ]
