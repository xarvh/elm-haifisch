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







size = 1




-- FUNCTIONS
addShip : Game -> Game
addShip game =
    let
        ship = Ship game.nextId 0 (vector (size/2) (size/2)) 0 []
    in
        { game
        | nextId = game.nextId + 1
        , ships = ship :: game.ships
        }





init =
    addShip <| Game 1 [] [] 0



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
                        -- TODO: should probably run some checks?
                        mapShip ship =
                            if ship.empireId /= empireId
                            then ship
                            else { ship | commands = [Ship.ThrustTo position] }
                    in
                        { model | ships = List.map mapShip model.ships }

