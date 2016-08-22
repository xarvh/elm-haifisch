
import Client
import Html.App as App


main = App.program
    { init = Client.init
    , update = Client.update
    , subscriptions = Client.subscriptions
    , view = Client.view
    }

