module GameMain exposing (..)

import GameFleet as Fleet

import GameCommon as G exposing
    ( Game
    , Empire, EmpireId
    , Fleet, FleetId
    , Vector, vector
    , Command
    , GameEffect
    )



-- TYPES




type Message
    = Tick
    | EmpireCommands EmpireId Command





-- FUNCTIONS
addFleet : Float -> Float -> Game -> Game
addFleet x y game =
    let
        fleet =
            Fleet.init game.nextId 0 (vector x y)
    in
        { game
        | nextId = game.nextId + 1
        , fleets = fleet :: game.fleets
        }





init =
    Game 1 [] [] 0 False
    |> addFleet (1/2) (1/2)
    |> addFleet (1/3) (1/3)
    |> addFleet (1/2) (1/3)







-- GAME EFFECTS


mergeFleetEffect game disappearingFleet remainingFleet =
    let
        newRemainingFleet =
            { remainingFleet | ships = List.append remainingFleet.ships disappearingFleet.ships }

        foldFleets fleet fleets =
            if fleet.id == disappearingFleet.id
            then fleets
            else (if fleet.id == newRemainingFleet.id then newRemainingFleet else fleet) :: fleets
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





-- GAME TICK


tick : Game -> Game
tick oldGame =
    let
        fleetFolder : Fleet -> (List Fleet, List GameEffect) -> ( List Fleet, List GameEffect )
        fleetFolder fleet (fleets, effects) =
            let
                ( newFleet, newEffects ) =
                    Fleet.tick oldGame fleet
            in
                ( newFleet :: fleets, List.append effects newEffects )

        ( newFleets, effects ) =
            List.foldr fleetFolder ([], []) oldGame.fleets


        newGame =
            { oldGame | fleets = newFleets }

    in
        List.foldl executeEffect newGame effects



-- UPDATE


update : Message -> Game -> Game
update message model =
    case message of
        Tick ->
            if model.pause then model else tick model

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

                G.TogglePause ->
                    { model | pause = not model.pause }
