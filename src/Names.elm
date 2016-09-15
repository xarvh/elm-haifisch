module Names exposing (..)





import Dict exposing (Dict)
import Random.Pcg as Random exposing (Generator)
import String


-- import Regex exposing (All, regex, )




type alias Definition =
    List String

type alias Lexicon =
    Dict String (List Definition)



{-

    {
        "fleet": [
            "$adj Murder",
            "$adj Fleet"
        ],
        "$adj": [
            "Efferate",
            "Merciless"
        ]
    }


-}







generateName : Lexicon -> String -> Generator String
generateName lexicon key =

    case Dict.get key lexicon of

        Nothing ->
            -- either the key is plain invalid, or it is stuck in a loop
            Random.constant <| if Dict.isEmpty lexicon then "-O-" else "-I-"

        Just definitions ->
            let

                reducedLexicon =
                    Dict.remove key lexicon


                -- TODO: PR to mgold/elm-random-pcb to add a mapN : (List a -> b) -> List (Generator a) -> Generator b
                foldDefinition definitionFragment =
                    Random.map2 (++) <|
                        case String.uncons definitionFragment of
                            Just ( '$', key ) -> generateName reducedLexicon key
                            _ -> Random.constant definitionFragment


                definitionToGenerator : Definition -> Generator String
                definitionToGenerator definition =
                    List.foldl foldDefinition (Random.constant "") definition


                generator =
                    Random.choices <| List.map definitionToGenerator definitions
            in
                generator


