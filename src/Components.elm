module Components exposing (..)

import Dict


type alias EntityId =
    Int


type alias Dict a =
    Dict.Dict EntityId a


get : Dict a -> EntityId -> Maybe a
get =
    flip Dict.get


get1 : Game -> (Game -> Dict a) -> EntityId -> Maybe a
get1 game component id =
    Dict.get id (component game)


get2 : Game -> ( Game -> Dict a, Game -> Dict b ) -> EntityId -> Maybe ( a, b )
get2 game ( a, b ) id =
    Maybe.map2 (,)
        (Dict.get id (a game))
        (Dict.get id (b game))


get3 : Game -> ( Game -> Dict a, Game -> Dict b, Game -> Dict c ) -> EntityId -> Maybe ( a, b, c )
get3 game ( a, b ) id =
    Maybe.map3 (,,)
        (Dict.get id (a game))
        (Dict.get id (b game))
        (Dict.get id (c game))
