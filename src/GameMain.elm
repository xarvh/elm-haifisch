module GameMain exposing (..)


import GameCommon exposing (..)

import GameShip as Ship
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



type Command
    = ShipMove






-- FUNCTIONS



init =
    Game 1 [] [] 0



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
            model

