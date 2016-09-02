module GameMain exposing (..)


import GameCommon as G exposing (Id, EmpireId, FleetId, Vector, vector, Command)

import GameFleet as Fleet exposing (Fleet, Ship)
import GameEmpire as Empire




-- TYPES



type alias Game =
    { nextId : Id
    , empires : List Empire.Empire
    , fleets : List Fleet
    , ticksSinceStart : Int
    }



type Message
    = Tick
    | EmpireCommands EmpireId Command





-- FUNCTIONS
addFleet : Float -> Float -> Game -> Game
addFleet x y game =
    let
        pos =
            (vector x y)

        ship =
            Ship pos pos 0 False

        fleet =
            Fleet game.nextId 0 (List.repeat 30 ship) []
    in
        { game
        | nextId = game.nextId + 1
        , fleets = fleet :: game.fleets
        }





init =
    Game 1 [] [] 0
    |> addFleet (1/2) (1/2)
--     |> addFleet (1/3) (1/3)
--     |> addFleet (1/2) (1/3)



tick : Game -> Game
tick model =
    { model
    | fleets = List.map Fleet.tick model.fleets
    }



update : Message -> Game -> Game
update message model =
    case message of
        Tick ->
            tick model

        EmpireCommands empireId command ->
            case command of
                G.FleetCommand fleetIds queueMode fleetCommand ->
                    let
                        updateCommands cmds =
                            case queueMode of
                                G.Append -> cmds ++ [fleetCommand]
                                G.Replace -> [fleetCommand]

                        mapFleet fleet =
                            if fleet.empireId /= empireId || not (List.member fleet.id fleetIds)
                            then fleet
                            else { fleet | commands = updateCommands fleet.commands }
                    in
                        { model | fleets = List.map mapFleet model.fleets }

