
import Client.Main as Client
import Navigation
import String



locationParser : Navigation.Location -> Maybe String
locationParser location =
    case String.dropLeft 1 location.hash of
        "" -> Nothing
        server -> Just server

parser : Navigation.Parser (Maybe String)
parser =
    (Navigation.makeParser locationParser)

main =
    Navigation.programWithFlags
        parser
        { init = Client.init
        , update = Client.update
        , urlUpdate = Client.urlUpdate
        , subscriptions = Client.subscriptions
        , view = Client.view
        }
