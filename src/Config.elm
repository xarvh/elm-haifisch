module Config exposing (..)

import App
import Gamepad exposing (Gamepad, Destination(..))
import Gamepad.Remap
import GamepadPort
import Html exposing (..)
import Html.Attributes exposing (class, disabled, selected, value)
import Html.Events
import Json.Decode
import Keyboard
import LocalStoragePort
import MousePort
import Input
import Time exposing (Time)


-- types


type alias Flags =
    { gamepadDatabaseAsString : String
    , gamepadDatabaseKey : String
    , dateNow : Int
    }


type ConfigModal
    = Main
    | Message String
    | Remapping (Gamepad.Remap.Model String)


type alias Model =
    { app : App.Model
    , gamepadDatabase : Gamepad.Database
    , gamepadDatabaseKey : String -- This is the key we use for the database in the browser's local storage
    , hasGamepads : Bool
    , hasKnownGamepads : Bool
    , maybeInputConfig : Maybe Input.Config
    , maybeModal : Maybe ConfigModal
    }


type Msg
    = OnAppMsg App.Msg
    | OnGamepad ( Time, Gamepad.Blob )
    | OnKey Int
    | OnInputConfig String
    | OnRemapButton
    | OnRemapMsg Gamepad.Remap.Msg



-- Remap targets


gamepadControls =
    [ ( A, "Fire" )
    , ( B, "Fire (different button)" )
    , ( LeftLeft, "Move left" )
    , ( LeftRight, "Move right" )
    , ( LeftUp, "Move up" )
    , ( LeftDown, "Move down" )
    , ( RightLeft, "Aim left" )
    , ( RightRight, "Aim right" )
    , ( RightUp, "Aim up" )
    , ( RightDown, "Aim down" )
    ]



-- init


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( app, appCmd ) =
            App.init flags.dateNow

        gamepadDatabase =
            flags.gamepadDatabaseAsString
                |> Gamepad.databaseFromString
                |> Result.withDefault Gamepad.emptyDatabase

        model =
            { app = app
            , gamepadDatabase = gamepadDatabase
            , gamepadDatabaseKey = flags.gamepadDatabaseKey
            , hasGamepads = False
            , hasKnownGamepads = False
            , maybeInputConfig = Nothing
            , maybeModal = Just Main
            }

        cmd =
            Cmd.map OnAppMsg appCmd
    in
        ( model, cmd )



-- update


noCmd model =
    ( model, Cmd.none )


updateRemap : Gamepad.Remap.Msg -> Gamepad.Remap.Model String -> Model -> ( Model, Cmd Msg )
updateRemap remapMsg remapModel model =
    case Gamepad.Remap.update remapMsg remapModel of
        Gamepad.Remap.StillOpen newModel ->
            noCmd { model | maybeModal = Just <| Remapping newModel }

        Gamepad.Remap.Error message ->
            noCmd { model | maybeModal = Just <| Message message }

        Gamepad.Remap.UpdateDatabase updateDatabase ->
            let
                gamepadDatabase =
                    updateDatabase model.gamepadDatabase

                cmd =
                    gamepadDatabase
                        |> Gamepad.databaseToString
                        |> LocalStoragePort.set model.gamepadDatabaseKey

                newModel =
                    { model
                        | maybeModal = Just <| Message "Gamepad configured!"
                        , gamepadDatabase = gamepadDatabase
                    }
            in
                ( newModel, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnAppMsg appMsg ->
            App.update
                (model.maybeModal /= Nothing)
                { maybeInputConfig = model.maybeInputConfig
                , gamepadDatabase = model.gamepadDatabase
                }
                appMsg
                model.app
                |> Tuple.mapFirst (\app -> { model | app = app })
                |> Tuple.mapSecond (Cmd.map OnAppMsg)

        OnGamepad ( dt, blob ) ->
            let
                knownGamepads =
                    blob
                        |> Gamepad.getGamepads model.gamepadDatabase
                        |> List.length

                allGamepads =
                    blob
                        |> Gamepad.getAllGamepadsAsUnknown
                        |> List.length
            in
                noCmd
                    { model
                        | hasGamepads = allGamepads > 0
                        , hasKnownGamepads = knownGamepads > 0
                    }

        OnKey code ->
            case code of
                -- Esc button
                27 ->
                    noCmd
                        { model
                            | maybeModal =
                                case model.maybeModal of
                                    Nothing ->
                                        Just Main

                                    _ ->
                                        Nothing
                        }

                _ ->
                    noCmd model

        OnInputConfig value ->
            noCmd
                { model
                    | maybeInputConfig =
                        case value of
                            "key" ->
                                Just Input.OnePlayerUsesKeyboardAndMouse

                            "pad" ->
                                Just Input.AllPlayersUseGamepads

                            _ ->
                                Nothing
                }

        OnRemapButton ->
            -- TODO: remove hardcoded gamepad index
            noCmd { model | maybeModal = Just <| Remapping <| Gamepad.Remap.init 0 gamepadControls }

        OnRemapMsg remapMsg ->
            case model.maybeModal of
                Just (Remapping remapModel) ->
                    updateRemap remapMsg remapModel model

                _ ->
                    noCmd model



-- view


viewInputConfig : Bool -> Maybe Input.Config -> Html Msg
viewInputConfig hasKnownGamepads maybeInputConfig =
    div
        []
        [ div [] [ text "Use keyboard?" ]
        , select
            [ Html.Events.on "change" (Json.Decode.map OnInputConfig Html.Events.targetValue)
            , disabled <| not hasKnownGamepads
            ]
            [ option
                [ value ""
                , selected <| maybeInputConfig == Nothing
                ]
                [ text "Guess" ]
            , option
                [ value "key"
                , selected <| maybeInputConfig == Just Input.OnePlayerUsesKeyboardAndMouse
                ]
                [ text "A player uses the keyboard" ]
            , option
                [ value "pad"
                , selected <| maybeInputConfig == Just Input.AllPlayersUseGamepads
                ]
                [ text "Everyone uses only gamepads" ]
            ]
        ]


viewConfig : Model -> Html Msg
viewConfig model =
    div
        []
        [ div
            [ class "ConfigModal-Item" ]
            [ text "Press Esc to to toggle Menu" ]
        , div
            [ class "ConfigModal-Item" ]
            [ viewInputConfig model.hasKnownGamepads model.maybeInputConfig ]
        , div
            [ class "ConfigModal-Item" ]
            [ button
                [ disabled <| not model.hasGamepads
                , Html.Events.onClick OnRemapButton
                ]
                [ text "Remap gamepads" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div
        [ class "root" ]
        [ App.view model.app |> Html.map OnAppMsg
        , case model.maybeModal of
            Nothing ->
                text ""

            Just configModel ->
                div
                    [ class "ConfigModal-Container" ]
                    [ div
                        [ class "ConfigModal-Content" ]
                        [ case configModel of
                            Main ->
                                viewConfig model

                            Message m ->
                                text m

                            Remapping remapModel ->
                                Gamepad.Remap.view remapModel |> text
                        ]
                    ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups OnKey
        , App.subscriptions model.app |> Sub.map OnAppMsg
        , case model.maybeModal of
            Nothing ->
                Sub.none

            Just Main ->
                GamepadPort.gamepad OnGamepad

            Just (Remapping remap) ->
                Gamepad.Remap.subscriptions GamepadPort.gamepad |> Sub.map OnRemapMsg

            Just (Message m) ->
                Sub.none
        ]



-- program


programWithFlags =
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }
