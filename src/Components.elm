module Components exposing (..)

import Dict exposing (Dict)


type alias EntityId =
    Int


type alias Components a =
    Dict EntityId a


get : Components a -> EntityId -> Maybe a
get =
    flip Dict.get


get1 : game -> (game -> Components a) -> EntityId -> Maybe a
get1 game component id =
    Dict.get id (component game)


get2 : game -> ( game -> Components a, game -> Components b ) -> EntityId -> Maybe ( a, b )
get2 game ( a, b ) id =
    Maybe.map2 (,)
        (Dict.get id (a game))
        (Dict.get id (b game))


get3 : game -> ( game -> Components a, game -> Components b, game -> Components c ) -> EntityId -> Maybe ( a, b, c )
get3 game ( a, b, c ) id =
    Maybe.map3 (,,)
        (Dict.get id (a game))
        (Dict.get id (b game))
        (Dict.get id (c game))


all1 : game -> (game -> Components a) -> List ( EntityId, a )
all1 game a =
    (a game) |> Dict.toList


all2 : game -> ( game -> Components a, game -> Components b ) -> List ( EntityId, a, b )
all2 game ( a, b ) =
    let
        extendTuple ( id, aa ) =
            Dict.get id (b game) |> Maybe.map ((,,) id aa)
    in
        all1 game a
            |> List.filterMap extendTuple


all3 : game -> ( game -> Components a, game -> Components b, game -> Components c ) -> List ( EntityId, a, b, c )
all3 game ( a, b, c ) =
    let
        extendTuple ( id, aa ) =
            Maybe.map2 ((,,,) id aa)
                (Dict.get id (b game))
                (Dict.get id (c game))
    in
        all1 game a
            |> List.filterMap extendTuple


all4 : game -> ( game -> Components a, game -> Components b, game -> Components c, game -> Components d ) -> List ( EntityId, a, b, c, d )
all4 game ( a, b, c, d ) =
    let
        extendTuple ( id, aa ) =
            Maybe.map3 ((,,,,) id aa)
                (Dict.get id (b game))
                (Dict.get id (c game))
                (Dict.get id (d game))
    in
        all1 game a
            |> List.filterMap extendTuple
