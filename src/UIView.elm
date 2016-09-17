module UIView exposing (view)


import GameCommon as G exposing
    ( Game
    , starSystemOuterRadius
    , vectorToString
    , normalizeBox
    , vector
    )

import Html as H
import Html.Attributes as HA
import StarSystemView
import Svg
import Svg.Attributes as SA
import UI
import FleetView



menuShip fleet ship =
    H.div
        [ HA.class "selection-ship" ]
        [ Svg.svg
            [ SA.width "8vh"
            , SA.height "8vh"
            , SA.viewBox "-1 -1 2 2"
            ]
            [ FleetView.shipSvg False ship ]
        , H.span [ HA.class "ship-name" ] [ H.text ship.name ]
        ]



menuFleet fleet =
    H.div
        [ HA.class "selection-fleet" ]
        [ H.div
            [ HA.class "fleet-header" ]
            [ H.span
                [ HA.class "fleet-name" ]
                [ H.text fleet.name ]
            , H.button
                []
                [ H.text "split" ]
            ]


        , H.div [ HA.class "selection-ships-list" ] <| List.map (menuShip fleet) fleet.ships
        ]



-- This gives info on whatever is selected
selectionMenu viewerPlayerId game ui =
    case ui.selectionType of
        UI.FleetSelection ->
            let
                selectedFleets =
                    List.filterMap (G.mapId game.fleets) ui.selectedIds
            in
                H.div
                    [ HA.class "selection-fleets-list" ]
                    (List.map menuFleet selectedFleets)




starSystemBox viewerPlayerId game ui =
    H.div
        [ HA.class "star-system-container" ]
        [ StarSystemView.view viewerPlayerId game ui ]





view : Int -> Game -> UI.Model -> H.Html UI.Msg
view viewerPlayerId game ui =
    H.div
        [ HA.style
            [ ( "width", "100vw" )
            , ( "height", "100vh" )
            ]
        ]
        [ starSystemBox viewerPlayerId game ui
        , selectionMenu viewerPlayerId game ui
        ]
