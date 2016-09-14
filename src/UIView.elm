module UIView exposing (view)


import GameCommon as G exposing
    ( Game
    , starSystemOuterRadius
    , vectorToString
    , normalizeBox
    , vector
    )

import Html
import Html.Attributes
import StarSystemView
import UI






view : Int -> Game -> UI.Model -> Html.Html UI.Msg
view viewerPlayerId game ui =
    Html.div
        [ Html.Attributes.style
            [ ( "position", "absolute" )
            , ( "width", "100wh" )
            , ( "height", "100vh" )
            , ( "top", "50%" )
            , ( "left", "50%" )
            , ( "transform", "translateX(-50%) translateY(-50%)" )
            ]
        ]
        [ StarSystemView.view viewerPlayerId game ui
        ]
