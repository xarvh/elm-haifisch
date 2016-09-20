module GameMain exposing (..)

import Dict exposing (Dict)
import FleetGame as Fleet
import Random.Pcg as Random
import Names
import GameCommon as G
    exposing
        ( Game
        , Empire
        , EmpireId
        , Fleet
        , FleetId
        , Vector
        , vector
        , Command
        , GameEffect
        )
import Set


-- TYPES


type Message
    = Tick
    | EmpireCommands EmpireId Command



-- FUNCTIONS


addFleet : EmpireId -> Float -> Float -> Game -> Game
addFleet empireId x y game =
    let
        ( fleet, nextId, seed ) =
            Fleet.init empireId (vector x y) ( game.nextId, game.seed )
    in
        { game
            | nextId = nextId
            , seed = seed
            , fleets = G.updateFleet fleet game.fleets
        }


init seed =
    Game 1 Dict.empty Dict.empty 0 False (Random.initialSeed seed)
        |> addFleet 0 (1 / 2) (1 / 2)
        |> addFleet 0 (1 / 3) (1 / 3)
        |> addFleet 0 (1 / 2) (1 / 3)
        |> addFleet 1 (-1 / 2) (-1 / 3)



-- GAME EFFECTS


mergeFleetEffect game disappearingFleet remainingFleet =
    let
        newRemainingFleet =
            { remainingFleet | ships = List.append remainingFleet.ships disappearingFleet.ships }

        newFleets =
            game.fleets
                |> Dict.remove disappearingFleet.id
                |> G.updateFleet newRemainingFleet
    in
        { game | fleets = newFleets }


executeEffect effect oldGame =
    case effect of
        G.MergeFleets disappearingFleetId remainingFleetId ->
            Maybe.map2
                (mergeFleetEffect oldGame)
                (Dict.get disappearingFleetId oldGame.fleets)
                (Dict.get remainingFleetId oldGame.fleets)
                |> Maybe.withDefault oldGame


splitFleet : G.FleetId -> Set.Set G.FleetId -> Game -> ( Game, List G.Notification)
splitFleet fleetId shipIds oldGame =
    case Dict.get fleetId oldGame.fleets of
        Nothing ->
            noNote oldGame

        Just oldRemainingFleet ->
            let
                ( leavingShips, remainingShips ) =
                    List.partition (\ship -> Set.member ship.id shipIds) oldRemainingFleet.ships
            in
                if List.isEmpty leavingShips || List.isEmpty remainingShips then
                    noNote oldGame
                else
                    let
                        ( leavingFleetName, seed0 ) =
                            Random.step Names.fleet oldGame.seed

                        ( leavingFleetId, nextId0 ) =
                            ( oldGame.nextId, oldGame.nextId + 1 )

                        newRemainingFleet =
                            { oldRemainingFleet
                                | ships = remainingShips
                            }

                        newLeavingFleet =
                            { oldRemainingFleet
                                | id = leavingFleetId
                                , name = leavingFleetName
                                , ships = leavingShips
                            }

                        newFleets =
                            oldGame.fleets
                                |> Dict.insert newRemainingFleet.id newRemainingFleet
                                |> Dict.insert newLeavingFleet.id newLeavingFleet
                    in
                        ( { oldGame | nextId = nextId0, seed = seed0, fleets = newFleets }, [ G.FleetHasSplit fleetId leavingFleetId ] )



-- GAME TICK


tick : Game -> Game
tick oldGame =
    let
        fleetFolder : FleetId -> Fleet -> ( G.FleetDict, List GameEffect ) -> ( G.FleetDict, List GameEffect )
        fleetFolder id fleet ( fleets, effects ) =
            let
                ( newFleet, newEffects ) =
                    Fleet.tick oldGame fleet
            in
                ( Dict.insert newFleet.id newFleet fleets, List.append effects newEffects )

        ( newFleets, effects ) =
            Dict.foldl fleetFolder ( Dict.empty, [] ) oldGame.fleets

        newGame =
            { oldGame | fleets = newFleets }
    in
        List.foldl executeEffect newGame effects



-- UPDATE


noNote model =
    ( model, [] )


update : Message -> Game -> ( Game, List G.Notification )
update message model =
    case message of
        Tick ->
            noNote <|
                if model.pause then
                    model
                else
                    tick model

        EmpireCommands empireId command ->
            case command of
                G.FleetCommand fleetIds queueMode fleetCommand ->
                    let
                        updateCommands cmds =
                            case queueMode of
                                G.Append ->
                                    cmds ++ [ fleetCommand ]

                                G.Replace ->
                                    [ fleetCommand ]

                        mapFleet id fleet =
                            if fleet.empireId /= empireId || not (Set.member fleet.id fleetIds) then
                                fleet
                            else
                                { fleet | commands = updateCommands fleet.commands }
                    in
                        noNote { model | fleets = Dict.map mapFleet model.fleets }

                G.FleetSplit fleetId shipIds ->
                    splitFleet fleetId shipIds model

                G.TogglePause ->
                    noNote { model | pause = not model.pause }
