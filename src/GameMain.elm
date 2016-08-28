module GameMain exposing (..)


import GameCommon exposing (Id, EmpireId, ShipId, Vector, vector, Command (ShipMove))

import GameShip as Ship exposing (Ship)
import GameEmpire as Empire




-- TYPES



type alias Game =
    { nextId : Id
    , empires : List Empire.Empire
    , ships : List Ship.Ship
    , ticksSinceStart : Int
    }



type Message
    = Tick
    | EmpireCommands EmpireId Command





-- FUNCTIONS
addShip : Float -> Float -> Game -> Game
addShip x y game =
    let
        ship = Ship game.nextId 0 (vector x y) 0 []
    in
        { game
        | nextId = game.nextId + 1
        , ships = ship :: game.ships
        }





init =
    Game 1 [] [] 0
    |> addShip (1/2) (1/2)
    |> addShip (1/3) (1/3)
    |> addShip (1/2) (1/3)



tick : Game -> Game
tick model =
    { model
    | ships = List.map Ship.tick model.ships
    }



update : Message -> Game -> Game
update message model =
    case message of
        Tick ->
            tick model

        EmpireCommands empireId command ->
            case command of
                ShipMove shipIds position ->
                    let
                        mapShip ship =
                            if ship.empireId /= empireId || not (List.member ship.id shipIds)
                            then ship
                            else { ship | commands = [Ship.ThrustTo position] }
                    in
                        { model | ships = List.map mapShip model.ships }

