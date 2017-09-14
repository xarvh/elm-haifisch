module Components exposing (..)

import Dict exposing (Dict)


type alias EntityId =
    Int


type alias ById a =
    Dict EntityId a


get : ById a -> EntityId -> Maybe a
get =
    flip Dict.get


get1 : game -> (game -> ById a) -> EntityId -> Maybe a
get1 game component id =
    Dict.get id (component game)


get2 : game -> ( game -> ById a, game -> ById b ) -> EntityId -> Maybe ( a, b )
get2 game ( a, b ) id =
    Maybe.map2 (,)
        (Dict.get id (a game))
        (Dict.get id (b game))


get3 : game -> ( game -> ById a, game -> ById b, game -> ById c ) -> EntityId -> Maybe ( a, b, c )
get3 game ( a, b, c ) id =
    Maybe.map3 (,,)
        (Dict.get id (a game))
        (Dict.get id (b game))
        (Dict.get id (c game))
