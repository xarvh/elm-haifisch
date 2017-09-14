module Game exposing (..)

import Array exposing (Array)


-- import Bots

import ColorPattern exposing (ColorPattern)
import Components exposing (EntityId)
import Dict
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import Random
import Random.Array


-- Types


type alias InputState =
    { finalAim : Vec2
    , fire : Bool
    , move : Vec2
    }


type InputDevice
    = InputDeviceKeyboardAndMouse
    | InputDeviceGamepad Int


type Controller
    = ControllerHuman InputDevice
    | ControllerBot Int


type alias PlayerComponent =
    { controller : Controller
    , inputState : Maybe InputState
    , score : Int
    , maybeShipId : Maybe EntityId
    }


type alias ShipComponent =
    { name : String
    , explodeTime : Time
    , reloadTime : Time
    , respawnTime : Time

    --, status : Status
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
    , cColorPattern : Components.ById ColorPattern
    , cHeading : Components.ById Float
    , cOrbit : Components.ById OrbitComponent
    , cOwner : Components.ById EntityId
    , cPlayer : Components.ById PlayerComponent
    , cPosition : Components.ById Vec2
    , cShip : Components.ById ShipComponent

    -- other stuff
    --, bots : Array Bots.Model
    , seed : Random.Seed
    , shuffledColorPatterns : Array ColorPattern
    }



-- Component Setters


sColorPattern : ColorPattern -> EntityId -> Game -> Game
sColorPattern component id game =
    { game | cColorPattern = Dict.insert id component game.cColorPattern }


sPlayer : PlayerComponent -> EntityId -> Game -> Game
sPlayer component id game =
    { game | cPlayer = Dict.insert id component game.cPlayer }



-- Entities


addEntity : List (EntityId -> Game -> Game) -> Game -> ( Game, EntityId )
addEntity componentSetters game =
    let
        newEntityId =
            game.lastEntityId + 1

        applySetter setter g =
            setter newEntityId g

        newGame =
            componentSetters
                |> List.foldl applySetter { game | lastEntityId = newEntityId }
    in
        ( newGame, newEntityId )


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



-- Init


init : Random.Seed -> Game
init seed =
    { lastEntityId = 0

    --
    , cColorPattern = Dict.empty
    , cHeading = Dict.empty
    , cOrbit = Dict.empty
    , cOwner = Dict.empty
    , cPlayer = Dict.empty
    , cPosition = Dict.empty
    , cShip = Dict.empty

    --
    --, bots = Array.empty
    , seed = seed
    , shuffledColorPatterns = Random.step (Random.Array.shuffle ColorPattern.patterns) seed |> Tuple.first
    }



-- Players


addPlayer : Controller -> Game -> ( Game, EntityId )
addPlayer controller game =
    let
        {-
           colorPattern =
             game.cPlayer
               |> Dict.keys
               |> List.filterMap (Component.get game.cColorPattern)
               |> Dict.Extra.groupBy identity
               |> Dict.map (always List.length)
        -}
        -- TODO
        colorPatternComponent =
            ColorPattern.neutral

        playerComponent =
            { controller = controller
            , inputState = Nothing
            , score = 0
            , maybeShipId = Nothing
            }

        components =
            [ sPlayer playerComponent
            , sColorPattern colorPatternComponent
            ]
    in
        addEntity components game
