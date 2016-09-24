
import Client.Main as Client
-- import Html.App as App
import Navigation



locationParser location =
    ""


urlUpdate data model =
    ( model, Cmd.none )



main =
    Navigation.programWithFlags
        (Navigation.makeParser locationParser)
        { init = \flags data -> Client.init flags
        , update = Client.update
        , urlUpdate = urlUpdate
        , subscriptions = Client.subscriptions
        , view = Client.view
        }
