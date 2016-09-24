module Client.UiCommon exposing (..)

import Game.Common as G
import Set


-- MODEL
-- These are all required by many different components


type alias UiShared a =
    { a
        | ctrl : Bool
        , shift : Bool
        , selection : Selection
    }


type Selection
    = NoSelection
    | FleetSelection (Set.Set G.Id)


type MouseButtonIndex
    = MouseLeft
    | MouseRight
    | MouseMid


type MouseButtonMovement
    = MousePress
    | MouseRelease


type Effect
    = Select Selection


queueMode : UiShared a -> G.QueueMode
queueMode uiShared =
    if uiShared.shift then
        G.Append
    else
        G.Replace


fleetIds : UiShared a -> Set.Set G.Id
fleetIds uiShared =
    case uiShared.selection of
        FleetSelection ids ->
            ids

        _ ->
            Set.empty

selectedFleets : UiShared a -> G.Game -> G.FleetDict
selectedFleets uiShared game =
    G.selectedFleets (fleetIds uiShared) game.fleets
