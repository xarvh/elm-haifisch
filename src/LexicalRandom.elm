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
import Random
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


constant : a -> Random.Generator a
constant value =
    Random.map (\_ -> value) Random.bool


combine : List (Random.Generator a) -> Random.Generator (List a)
combine =
    List.foldr (Random.map2 (::)) (constant [])


choices : Random.Generator a -> Array (Random.Generator a) -> Random.Generator a
choices default array =
    (Random.int 0 <| Array.length array - 1)
        `Random.andThen`
            \index ->
                Array.get index array
                    |> Maybe.withDefault default


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
generator : (String -> String) -> Lexicon -> String -> Random.Generator String
generator filler lexicon key =
    case Dict.get key lexicon of
        Nothing ->
            -- either the key is plain invalid, or it is stuck in a loop
            constant (filler key)

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
                            constant string

                definitionToGenerator definition =
                    List.map fragmentToGenerator definition
                        |> combine
                        |> Random.map (String.join "")
            in
                Array.map definitionToGenerator definitions
                    |> choices (constant "")



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
