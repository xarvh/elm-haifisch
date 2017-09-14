module Game exposing (..)

import Array exposing (Array)
import ColorPattern exposing (ColorPattern)
import Components exposing (EntityId)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import Random


-- Types


type InputDevice
    = InputDeviceKeyboardAndMouse
    | InputDeviceGamepad Int


type Controller
    = Human InputDevice
    | Bot Int


type alias PlayerComponent =
    { controller : Controller
    , inputState : InputState
    , isActive : Bool
    , score : Int
    , shipId : EntityId
    }


type alias ShipComponent =
    { name : String
    , explodeTime : Time
    , reloadTime : Time
    , respawnTime : Time
    , status : Status
    }


type alias OrbitComponent =
    { angularPosition : Float
    , angularSpeed : Float
    , orbitAnchorId : EntityId
    , orbitRadius : Float
    , surfaceRadius : Float
    }


type alias Game =
    { lastEntityId : EntityId

    -- components
    , cColorPattern : Components.Dict ColorPattern
    , cHeading : Components.Dict Float
    , cOrbit : Components.Dict OrbitComponent
    , cOwner : Components.Dict Id
    , cPlayer : Components.Dict PlayerComponent
    , cPosition : Components.Dict Vec2
    , cShip : Components.Dict ShipComponent

    -- other stuff
    , bots : Array Bots.Model
    , seed : Random.Seed
    , shuffledColorPatterns : Array ColorPattern
    }



-- Basic game functions


init : Random.Seed -> Game
init seed =
    { lastEntityId = 0

    --
    , cColorPattern = Dict.empty
    , cHeading = Dict.empty
    , cOrbit = Dict.empty
    , cOwner = Dict.empty
    , cPlayer = Components.Dict PlayerComponent
    , cPosition = Dict.empty
    , cShip = Dict.empty

    --
    , bots = Array.empty
    , seed = seed
    , shuffledColorPatterns = Random.step (Random.Array.shuffle ColorPattern.patterns) seed |> Tuple.first
    }


addEntity : Game -> ( Game, EntityId )
addEntity game =
    let
        newEntityId =
            game.lastEntityId + 1
    in
        ( { game | lastEntityId = newEntityId }, newEntityId )


removeEntity : EntityId -> Game -> Game
removeEntity id game =
    let
        removeFrom component =
            Dict.remove id (component game)
    in
        { game
            | cColorPattern = removeFrom .cColorPattern
            , cHeading = removeFrom .cHeading
            , cOrbit = removeFrom .cOrbit
            , cOwner = removeFrom .cOwner
            , cPlayer = removeFrom .cPlayer
            , cPosition = removeFrom .cPosition
            , cShip = removeFrom .cShip
        }
