module Game exposing (..)

import Array exposing (Array)
import Common
import ColorPattern exposing (ColorPattern)
import Components exposing (Components, EntityId)
import Dict exposing (Dict)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Time exposing (Time)
import Random
import Random.List


-- Player & Input


type Aim
    = AimRelative Vec2
    | AimAbsolute Vec2


type alias InputState =
    { finalAim : Aim
    , fire : Bool
    , move : Vec2
    }


neutralInputState =
    { finalAim = AimRelative (vec2 0 1)
    , fire = False
    , move = vec2 0 0
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
    }



-- Ship


type ShipStatus
    = ShipSpawning
    | ShipActive
    | ShipExploding


type alias ShipComponent =
    { name : String
    , explodeTime : Time
    , reloadTime : Time
    , respawnTime : Time
    , status : ShipStatus
    }



-- Orbit


type alias OrbitComponent =
    { angularPosition : Float
    , angularSpeed : Float
    , orbitAnchorId : EntityId
    , orbitRadius : Float
    , surfaceRadius : Float
    }



-- Game


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
    , seed : Random.Seed
    , shuffledColorPatterns : List ColorPattern
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
    , seed = seed
    , shuffledColorPatterns = Random.step (Random.List.shuffle ColorPattern.patterns) seed |> Tuple.first
    }



-- Add


addPlayer : Controller -> Game -> ( Game, EntityId )
addPlayer controller game =
    let
        players =
            Components.all2 game ( .cPlayer, .cColorPattern )

        colorPatternCount colorPattern =
            players
                |> List.filter (\( id, player, color ) -> color == colorPattern)
                |> List.length

        colorPattern =
            game.shuffledColorPatterns
                |> List.sortBy colorPatternCount
                |> List.head
                |> Maybe.withDefault ColorPattern.neutral

        player =
            { controller = controller
            , inputState = Nothing
            , score = 0
            }

        components =
            [ sPlayer player
            , sColorPattern colorPattern
            ]
    in
        addEntity components game


addShip : ( EntityId, PlayerComponent, ColorPattern ) -> Game -> Game
addShip ( ownerId, player, colorPattern ) game =
    let
        ( seed, ( position, name ) ) =
            -- TODO
            ( game.seed, ( vec2 0 0, "LOOL" ) )

        heading =
            Vec2.negate position |> Common.vectorToAngle

        ship =
            { name = name
            , explodeTime = 0
            , reloadTime = 0
            , respawnTime = 0
            , status = ShipSpawning
            }

        components =
            [ sColorPattern colorPattern
            , sHeading heading
            , sOwner ownerId
            , sPosition position
            , sShip ship
            ]
    in
        { game | seed = seed }
            |> addEntity components
            |> Tuple.first



-- Helpers


shipsByOwnerId : Game -> Dict EntityId ( EntityId, ShipComponent )
shipsByOwnerId game =
    Components.all2 game ( .cOwner, .cShip )
        |> List.map (\( shipId, ownerId, ship ) -> ( ownerId, ( shipId, ship ) ))
        |> Dict.fromList


shipOwner : Game -> EntityId -> Maybe PlayerComponent
shipOwner game shipId =
    Components.get game.cOwner shipId
        |> Maybe.andThen (Components.get1 game .cPlayer)
