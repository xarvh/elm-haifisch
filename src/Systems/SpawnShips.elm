module Systems.SpawnShips exposing (..)

import Components exposing (EntityId)
import Game exposing (Game)


spawnShipsForPlayersWithoutAShip : Game -> Game
spawnShipsForPlayersWithoutAShip game =
    let
        playerLacksShips : ( EntityId, Game.PlayerComponent, a ) -> Bool
        playerLacksShips ( id, player, _ ) =
            Maybe.andThen (Components.get game.cShip) player.maybeShipId == Nothing

        playersWithoutAShip =
            Components.all2 game ( .cPlayer, .cColorPattern )
                |> List.filter playerLacksShips

        addShipForPlayer ( id, player, colorPattern ) game =
            Game.addShip colorPattern id game |> Tuple.first
    in
        playersWithoutAShip
            |> List.foldl addShipForPlayer game
