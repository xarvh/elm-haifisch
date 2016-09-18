module UIView exposing (view)


import GameCommon as G exposing
    ( Game
    , Id, Fleet, Ship
    , starSystemOuterRadius
    , vectorToString
    , normalizeBox
    , vector
    )

import Html as H
import Html.Events as HE
import Html.Attributes as HA
import StarSystemView
import Svg
import Svg.Attributes as SA
import Set
import UI
import FleetView



menuShip : Bool -> Set.Set Id -> Ship -> H.Html UI.Msg
menuShip isSplittingFleet splittingShipIds ship =
    H.div
        [ HA.class "selection-ship" ]
        [ Svg.svg
            [ SA.width "8vh"
            , SA.height "8vh"
            , SA.viewBox "-1 -1 2 2"
            ]
            [ FleetView.shipSvg False ship ]
        , H.span [ HA.class "ship-name" ] [ H.text ship.name ]
        , if not isSplittingFleet
            then H.text ""
            else
                H.span
                    [ HE.onClick (UI.UserClicksShipSplit ship.id) ]
                    [ H.text <| if Set.member ship.id splittingShipIds then "go to new fleet" else "stay" ]
        ]


menuFleet : UI.Model -> Fleet -> H.Html UI.Msg
menuFleet ui fleet =
    let
        (splittingFleetId, splittingShipIds) = Maybe.withDefault (-1, Set.empty) ui.splittingFleet
        isSplitting = fleet.id == splittingFleetId
    in
      H.div
        [ HA.class "selection-fleet" ]
        [ H.div
            [ HA.class "fleet-header" ]
            [ H.span
                [ HA.class "fleet-name" ]
                [ H.text fleet.name ]
            , H.button
                [ HE.onClick (UI.UserClicksFleetSplit fleet.id) ]
                [ H.text <| if isSplitting then "Done" else "Split" ]
            ]


        , H.div [ HA.class "selection-ships-list" ] <| List.map (menuShip isSplitting splittingShipIds) fleet.ships
        ]



-- This gives info on whatever is selected
selectionMenu : Id -> G.Game -> UI.Model -> H.Html UI.Msg
selectionMenu viewerPlayerId game ui =
    case ui.selectionType of
        UI.FleetSelection ->
            let
                selectedFleets =
                    List.filterMap (G.mapId game.fleets) ui.selectedIds
            in
                H.div
                    [ HA.class "selection-fleets-list" ]
                    (List.map (menuFleet ui) selectedFleets)




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
