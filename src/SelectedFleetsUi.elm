module SelectedFleetsUi exposing (..)

import GameCommon as G
    exposing
        ( Game
        , Id
        , Fleet
        , Ship
        )
import Html as H
import Html.Events as HE
import Html.Attributes as HA
import Svg
import Svg.Attributes as SA
import Set
import FleetView


type alias Model =
    -- Just fleetId, idOfShipsToBeExtracted
    { splittingFleet : Maybe ( Id, Set.Set Id )
    }


init =
    Model Nothing


type Msg
    = UserClicksFleetSplit Id
    | UserClicksShipSplit Id



-- UPDATE


noCmd model =
    ( model, [] )


toggle id set =
    (if Set.member id set then
        Set.remove
     else
        Set.insert
    )
        id
        set


update : Msg -> Model -> ( Model, List G.Command )
update msg model =
    case msg of
        UserClicksFleetSplit clickedFleetId ->
            case Maybe.withDefault ( -1, Set.empty ) model.splittingFleet of
                ( selectedFleetId, shipIds ) ->
                    if selectedFleetId /= clickedFleetId then
                        noCmd { model | splittingFleet = Just ( clickedFleetId, Set.empty ) }
                    else
                        let
                            newModel =
                                { model | splittingFleet = Nothing }

                            cmd =
                                if Set.isEmpty shipIds then
                                    []
                                else
                                    [ G.FleetSplit clickedFleetId (Set.toList shipIds) ]
                        in
                            ( newModel, cmd )

        UserClicksShipSplit shipId ->
            noCmd <|
                case model.splittingFleet of
                    Nothing ->
                        model

                    Just ( fId, shipIds ) ->
                        { model | splittingFleet = Just ( fId, toggle shipId shipIds ) }



-- VIEW


menuShip : Bool -> Bool -> Set.Set Id -> Ship -> H.Html Msg
menuShip isFriendly isSplittingFleet splittingShipIds ship =
    H.div
        [ HA.class "selection-ship" ]
        [ Svg.svg
            [ SA.width "8vh"
            , SA.height "8vh"
            , SA.viewBox "-1 -1 2 2"
            ]
            [ FleetView.shipSvg isFriendly False ship ]
        , H.span [ HA.class "ship-name" ] [ H.text ship.name ]
        , if not isSplittingFleet then
            H.text ""
          else
            H.span
                [ HE.onClick (UserClicksShipSplit ship.id) ]
                [ H.text <|
                    if Set.member ship.id splittingShipIds then
                        "go to new fleet"
                    else
                        "stay"
                ]
        ]


menuFleet : Id -> Model -> Fleet -> H.Html Msg
menuFleet viewerPlayerId model fleet =
    let
        ( splittingFleetId, splittingShipIds ) =
            Maybe.withDefault ( -1, Set.empty ) model.splittingFleet

        isSplitting =
            fleet.id == splittingFleetId

        isFriendly =
            fleet.empireId == viewerPlayerId
    in
        H.div
            [ HA.class "selection-fleet" ]
            [ H.div
                [ HA.class "fleet-header" ]
                [ H.span
                    [ HA.class "fleet-name" ]
                    [ H.text fleet.name ]
                , H.button
                    [ HE.onClick (UserClicksFleetSplit fleet.id) ]
                    [ H.text <|
                        if isSplitting then
                            "Done"
                        else
                            "Split"
                    ]
                ]
            , H.div [ HA.class "selection-ships-list" ] <|
                List.map (menuShip isFriendly isSplitting splittingShipIds) fleet.ships
            ]


view : Id -> List Fleet -> Model -> H.Html Msg
view viewerPlayerId selectedFleets model =
    H.div
        [ HA.class "selection-fleets-list" ]
        (List.map (menuFleet viewerPlayerId model) selectedFleets)
