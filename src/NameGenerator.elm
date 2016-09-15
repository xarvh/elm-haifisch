module NameGenerator exposing
    ( Definition, Lexicon
    , generator
    , capitalize
    , fromString
    )


import Dict exposing (Dict)
import Random.Pcg as Random exposing (Generator)
import String
import Regex as R



-- types

type Fragment
    = Constant String
    | Key String


type alias Definition =
    List Fragment


type alias Lexicon =
    Dict String (List Definition)



-- Generate a name given a lexicon and a key of that lexicon

generator : Lexicon -> String -> Generator String
generator lexicon key =

    case Dict.get key lexicon of

        Nothing ->
            -- either the key is plain invalid, or it is stuck in a loop
            Random.constant <| "--[" ++ key ++ "]--"

        Just definitions ->
            let
                reducedLexicon =
                    Dict.remove key lexicon


                -- TODO: PR to mgold/elm-random-pcb to add a mapN : (List a -> b) -> List (Generator a) -> Generator b
                foldDefinition definitionFragment accumulatedGenerator =
                    Random.map2 (++) accumulatedGenerator <|
                        case definitionFragment of
                            Key key -> generator reducedLexicon key
                            Constant string -> Random.constant string


                definitionToGenerator : Definition -> Generator String
                definitionToGenerator definition =
                    List.foldl foldDefinition (Random.constant "") definition
            in
                Random.choices <| List.map definitionToGenerator definitions



-- Capitalisation helper

capitalize : String -> String
capitalize =
    R.replace R.All (R.regex "\\b\\w") (.match >> String.toUpper)



-- Load a lexicon from a multi-line string

fromString : String -> Lexicon
fromString stringLexicon =
    let
        stringToDefinition stringDefinition =
            let
                chunkToFrags chunk =
                    case String.split "}" chunk of
                        key :: constant :: [] -> [ Key key, Constant constant ]
                        _ -> [Constant chunk]
            in
                stringDefinition
                |> String.split "{"
                |> List.map chunkToFrags
                |> List.concat


        addToLexiconKey key line lexicon =
            let
                newDefinitions =
                    line
                    |> String.split ","
                    |> List.map String.trim
                    |> List.filter ((/=) "")
                    |> List.map stringToDefinition

                existingDefinitions =
                    Dict.get key lexicon
                    |> Maybe.withDefault []
            in
                Dict.insert key (List.append existingDefinitions newDefinitions) lexicon


        addLine : String -> ( String, Lexicon ) -> ( String, Lexicon )
        addLine line ( currentKey, lexicon ) =
            if R.contains (R.regex "^\\s*#") line
            then ( currentKey, lexicon )
            else
                if R.contains (R.regex "^\\s") line
                then ( currentKey, addToLexiconKey currentKey line lexicon )
                else ( line, lexicon )
    in
        snd <| List.foldl addLine ("default", Dict.empty) (String.lines stringLexicon)
