module Systems.SpawnShips exposing (..)

import Components exposing (EntityId)
import Dict
import Game exposing (Game)


spawnShipsForPlayersWithoutAShip : Game -> Game
spawnShipsForPlayersWithoutAShip game =
    let
        shipsByOwnerId =
            Game.shipsByOwnerId game

        playerLacksShips ( id, player, _ ) =
            Dict.member id shipsByOwnerId

        playersWithoutAShip =
            Components.all2 game ( .cPlayer, .cColorPattern )
                |> List.filter playerLacksShips
    in
        playersWithoutAShip
            |> List.foldl Game.addShip game
