module GameMain exposing (..)

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
            , fleets = fleet :: game.fleets
        }


init seed =
    Game 1 [] [] 0 False (Random.initialSeed seed)
        |> addFleet 0 (1 / 2) (1 / 2)
        |> addFleet 0 (1 / 3) (1 / 3)
        |> addFleet 0 (1 / 2) (1 / 3)
        |> addFleet 1 (-1 / 2) (-1 / 3)



-- GAME EFFECTS


mergeFleetEffect game disappearingFleet remainingFleet =
    let
        newRemainingFleet =
            { remainingFleet | ships = List.append remainingFleet.ships disappearingFleet.ships }

        foldFleets fleet fleets =
            if fleet.id == disappearingFleet.id then
                fleets
            else
                (if fleet.id == newRemainingFleet.id then
                    newRemainingFleet
                 else
                    fleet
                )
                    :: fleets
    in
        { game | fleets = List.foldr foldFleets [] game.fleets }


executeEffect effect oldGame =
    case effect of
        G.MergeFleets disappearingFleetId remainingFleetId ->
            Maybe.map2
                (mergeFleetEffect oldGame)
                (G.findId disappearingFleetId oldGame.fleets)
                (G.findId remainingFleetId oldGame.fleets)
                |> Maybe.withDefault oldGame


splitFleet fleetId shipIds oldGame =
    case G.findId fleetId oldGame.fleets of
        Nothing ->
            noNote oldGame

        Just oldRemainingFleet ->
            case List.partition (\{ id } -> List.member id shipIds) oldRemainingFleet.ships of
                ( [], _ ) ->
                    noNote oldGame

                ( _, [] ) ->
                    noNote oldGame

                ( leavingShips, remainingShips ) ->
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
                            newRemainingFleet :: newLeavingFleet :: List.filter (\{ id } -> id /= oldRemainingFleet.id) oldGame.fleets
                    in
                        ( { oldGame | nextId = nextId0, seed = seed0, fleets = newFleets }, [ G.FleetHasSplit fleetId leavingFleetId ] )



-- GAME TICK


tick : Game -> Game
tick oldGame =
    let
        fleetFolder : Fleet -> ( List Fleet, List GameEffect ) -> ( List Fleet, List GameEffect )
        fleetFolder fleet ( fleets, effects ) =
            let
                ( newFleet, newEffects ) =
                    Fleet.tick oldGame fleet
            in
                ( newFleet :: fleets, List.append effects newEffects )

        ( newFleets, effects ) =
            List.foldr fleetFolder ( [], [] ) oldGame.fleets

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

                        mapFleet fleet =
                            if fleet.empireId /= empireId || not (List.member fleet.id fleetIds) then
                                fleet
                            else
                                { fleet | commands = updateCommands fleet.commands }
                    in
                        noNote { model | fleets = List.map mapFleet model.fleets }

                G.FleetSplit fleetId shipIds ->
                    splitFleet fleetId shipIds model

                G.TogglePause ->
                    noNote { model | pause = not model.pause }
