module GameCommon exposing (..)


import Math.Vector2 as V
import List.Extra
import Random.Pcg as Random
import Set


-- IDs


type alias Id = Int
type alias EmpireId = Id
type alias FleetId = Id
type alias ShipId = Id






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

    , ships : List Ship

    {- TODO
    Position =
        StarSystem starSystemId
        InFTL starSystemId StarSystemId completion
    -}

    -- formationDirection is to be updated whenever formationTarget chagnes
    , formationTarget : Vector
    , formationDirection : Vector

    , commands : List FleetCommand
    }



type alias Game =
    { nextId : Id
    , empires : List Empire
    , fleets : List Fleet
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
    | MergeWith FleetId



type Command
    = FleetCommand (List FleetId) QueueMode FleetCommand
    | FleetSplit FleetId (List ShipId)
    | TogglePause


-- This is the max distance a ship can be from a star outside FTL
starSystemOuterRadius : Float
starSystemOuterRadius =
    0.96




-- NOTIFICATIONS


type Notification
    = FleetHasSplit Id Id -- originalFleetId newFleetId






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
        ax = V.getX a
        ay = V.getY a
        bx = V.getX b
        by = V.getY b
    in
        ( min ax bx
        , min ay by
        , max ax bx
        , max ay by
        )



-- HELPERS


findId id =
    List.Extra.find (\item -> item.id == id)

-- Used to get from a list of ids to a list of items
-- `List.filterMap (G.mapId game.fleets) ui.selectedIds`
mapId =
    flip findId


selectedFleets selectedIds =
    List.filter (\item -> Set.member item.id selectedIds)
