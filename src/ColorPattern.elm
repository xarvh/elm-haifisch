module ColorPattern exposing (..)

import Array exposing (Array)


type alias ColorPattern =
    { bright : String
    , dark : String
    , key : String
    }


patterns : Array ColorPattern
patterns =
    Array.fromList
        [ { bright = "#f00", dark = "#900", key = "red" }
        , { bright = "#0f0", dark = "#090", key = "green" }
        , { bright = "#00f", dark = "#009", key = "blue" }
        , { bright = "#0ee", dark = "#0bb", key = "cyan" }
        , { bright = "#f0f", dark = "#909", key = "purple" }
        , { bright = "#ee0", dark = "#bb0", key = "yellow" }
        ]


neutral : ColorPattern
neutral =
    { bright = "#bbb", dark = "#999", key = "grey" }


get : Int -> Array ColorPattern -> ColorPattern
get index colorPatterns =
    colorPatterns
        |> Array.get (index % Array.length colorPatterns)
        |> Maybe.withDefault neutral
