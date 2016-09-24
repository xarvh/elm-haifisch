module Game.Common exposing (..)

import Dict exposing (Dict)
import Math.Vector2 as V
import List.Extra
import Random.Pcg as Random
import Set exposing (Set)


-- IDs


type alias Id =
    Int


type alias EmpireId =
    Id


type alias FleetId =
    Id


type alias ShipId =
    Id



-- Dictionaries


type alias FleetDict =
    Dict FleetId Fleet



-- MODELS


type alias Empire =
    { id : EmpireId
    , name : String
    }


type alias Ship =
    { id : ShipId
    , name : String
    , currentPosition : Vector
    , targetPosition : Vector
    , angle : Float
    , isThrusting : Bool
    }


type alias Fleet =
    { id : FleetId
    , name : String
    , empireId : EmpireId
    , ships :
        List Ship
        -- formationDirection is to be updated whenever formationTarget chagnes
    , formationTarget : Vector
    , formationDirection : Vector
    , commands :
        List FleetCommand
        -- TODO Add `Position = StarSystem starSystemId | InFTL starSystemId StarSystemId completion`
    }


type alias Game =
    { nextId : Id
    , empires : Dict EmpireId Empire
    , fleets : FleetDict
    , ticksSinceStart : Int
    , pause : Bool
    , seed : Random.Seed
    }



-- COMMANDS


type QueueMode
    = Append
    | Replace


type FleetCommand
    = ThrustTo Vector
    | Attack FleetId
    | MergeWith FleetId


type Command
    = FleetCommand (Set FleetId) QueueMode FleetCommand
    | FleetSplit FleetId (Set ShipId)
    | TogglePause



-- This is the max distance a ship can be from a star outside FTL


starSystemOuterRadius : Float
starSystemOuterRadius =
    0.96



-- NOTIFICATIONS


type Notification
    = FleetHasSplit Id Id



-- originalFleetId newFleetId
{- EFFECTS

   When a tick elapses, game components (so far only Fleets) can update their own model
   but cannot directly touch any other model.
   Instead, their update/tick function will produce an Effect, which describes a side
   effect that a game component can produce in the game model
-}


type GameEffect
    = MergeFleets FleetId FleetId



-- STAR SYSTEM COORDINATES


type alias Vector =
    V.Vec2


vector =
    V.vec2


vectorToString : Vector -> String
vectorToString v =
    toString (V.getX v) ++ "," ++ toString (V.getY v)



-- GEOMETRY


normalizeBox a b =
    let
        ax =
            V.getX a

        ay =
            V.getY a

        bx =
            V.getX b

        by =
            V.getY b
    in
        ( min ax bx
        , min ay by
        , max ax bx
        , max ay by
        )



-- HELPERS


selectedFleets : Set FleetId -> FleetDict -> FleetDict
selectedFleets selectedIds =
    Dict.filter <| \id fleet -> Set.member id selectedIds


updateFleet : Fleet -> FleetDict -> FleetDict
updateFleet fleet fleets =
    Dict.insert fleet.id fleet fleets