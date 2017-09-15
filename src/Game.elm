module Game exposing (..)

import Array exposing (Array)


-- import Bots

import ColorPattern exposing (ColorPattern)
import Components exposing (Components, EntityId)
import Dict
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import Random
import Random.Array


-- Stuff that probably shouldn't live here


worldRadius =
    1



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
    , cColorPattern : Components ColorPattern
    , cHeading : Components Float
    , cOrbit : Components OrbitComponent
    , cOwner : Components EntityId
    , cPlayer : Components PlayerComponent
    , cPosition : Components Vec2
    , cShip : Components ShipComponent

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


sHeading : Float -> EntityId -> Game -> Game
sHeading component id game =
    { game | cHeading = Dict.insert id component game.cHeading }


sOwner : EntityId -> EntityId -> Game -> Game
sOwner ownerId targetId game =
    { game | cOwner = Dict.insert targetId ownerId game.cOwner }


sPosition : Vec2 -> EntityId -> Game -> Game
sPosition component id game =
    { game | cPosition = Dict.insert id component game.cPosition }


sShip : ShipComponent -> EntityId -> Game -> Game
sShip component id game =
    { game | cShip = Dict.insert id component game.cShip }



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



-- Add


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


addShip : ColorPattern -> EntityId -> Game -> ( Game, EntityId )
addShip colorPattern ownerId game =
    let
        ( seed, ( position, name ) ) =
            -- TODO
            ( game.seed, ( vec2 0 0, "LOOL" ) )

        heading =
            -- TODO
            0

        ship =
            { name = name
            , explodeTime = 0
            , reloadTime = 0
            , respawnTime = 0
            }

        components =
            [ sColorPattern colorPattern
            , sHeading heading
            , sOwner ownerId
            , sPosition position
            , sShip ship
            ]
    in
        addEntity components { game | seed = seed }
