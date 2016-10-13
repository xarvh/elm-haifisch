module View exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Ship exposing (Ship)
import Svg as S
import Svg.Attributes as SA



background =
    let
        ( fill, stroke, strokeWidth ) =
            if True then
                ( "#000030", "#000075", "3px" )
            else
                ( "#000030", "black", "10px" )
    in
        S.svg
            [ SA.class "ui-background full-window"
            ]
            [ S.defs
                []
                [ S.pattern
                    [ SA.id "hex-background"
                    , SA.width "56px"
                    , SA.height "100px"
                    , SA.patternUnits "userSpaceOnUse"
                    , SA.patternTransform "scale(0.5)"
                    ]
                    [ S.rect
                        [ SA.width "100%"
                        , SA.height "100%"
                        , SA.fill fill
                        ]
                        []
                    , S.path
                        [ SA.d "M28 66L0 50L0 16L28 0L56 16L56 50L28 66L28 100"
                        , SA.fill fill
                        , SA.stroke stroke
                        , SA.strokeWidth strokeWidth
                        ]
                        []
                    ]
                ]
            , S.rect
                [ SA.fill "url(#hex-background)"
                , SA.width "100%"
                , SA.height "100%"
                ]
                []
            ]




starSystemBox : List Ship -> Html Never
starSystemBox ships =
    H.div
        [ HA.class "star-system-container full-window" ]
--         [ StarSystemUi.view viewerPlayerId game model model.starSystemUi
--             |> App.map ToStarSystemUiMsg
        []
