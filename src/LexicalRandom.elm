module LexicalRandom
    exposing
        ( Lexicon
        , generator
        , capitalize
        , fromString
        )

{-| Generate random names based on a lexicon.
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Random exposing (Generator)
import Random.Array
import Random.Extra
import String
import Regex as R


-- types


type Fragment
    = Constant String
    | Key String


type alias Definition =
    List Fragment


type alias Lexicon =
    Dict String (Array Definition)



-- Random helpers


choices : Generator a -> Array (Generator a) -> Generator a
choices default array =
    Random.Array.sample array
        `Random.andThen` (Maybe.withDefault default)


{-| Generate a name given a lexicon and a key of that lexicon

    `(String -> String)` is a filler function, called when some definition references a key
    that does not exist in the lexicon.
    It will be called with the missing key as argument, and its return value will be used
    as the key's value.
    This can be used, for example, to provide values from a custom dictionary.

    The filler function is also called to break possible infinite recursions caused by a key.


    filler key =
        Dict.get key customKeys |> Maybe.withDefault key

    nameGenerator =
        LexicalRandom.generator filler englishGibberishLexicon "properNoun"

    ( name, seed ) =
        Random.step nameGenerator seed
-}
generator : (String -> String) -> Lexicon -> String -> Generator String
generator filler lexicon key =
    case Dict.get key lexicon of
        Nothing ->
            -- either the key is plain invalid, or it is stuck in a loop
            Random.Extra.constant (filler key)

        Just definitions ->
            let
                -- Remove used keys to prevent infinite recursion
                reducedLexicon =
                    Dict.remove key lexicon

                fragmentToGenerator fragment =
                    case fragment of
                        Key key ->
                            generator filler reducedLexicon key

                        Constant string ->
                            Random.Extra.constant string

                definitionToGenerator definition =
                    List.map fragmentToGenerator definition
                        |> Random.Extra.together
                        |> Random.map (String.join "")
            in
                Array.map definitionToGenerator definitions
                    |> choices (Random.Extra.constant "")



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
                        key :: constant :: [] ->
                            [ Key key, Constant constant ]

                        _ ->
                            [ Constant chunk ]
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
                        |> Array.fromList

                existingDefinitions =
                    Dict.get key lexicon
                        |> Maybe.withDefault Array.empty
            in
                Dict.insert key (Array.append existingDefinitions newDefinitions) lexicon

        addLine : String -> ( String, Lexicon ) -> ( String, Lexicon )
        addLine line ( currentKey, lexicon ) =
            if R.contains (R.regex "^\\s*#") line then
                ( currentKey, lexicon )
            else if R.contains (R.regex "^\\s") line then
                ( currentKey, addToLexiconKey currentKey line lexicon )
            else
                ( line, lexicon )
    in
        snd <| List.foldl addLine ( "default", Dict.empty ) (String.lines stringLexicon)
