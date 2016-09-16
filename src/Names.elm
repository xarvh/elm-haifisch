module Names exposing (..)



import LexicalRandom
import Random.Pcg as Random



defaultLexicon =
    LexicalRandom.fromString """

properNoun
    # TODO: add more from https://en.wikipedia.org/wiki/Former_constellations
    Ty,Long,Wads,
    Amaat,Ente,Toren,Karl,Ilves,
    Vaassar,Maat,Morne,Ultema,Tahna,Ebek,Uvud,Iloe
    Auriga,Antilia,Acquila,Ara,Carina,Centaurus,Coma,Crux
    Eridanus,Hydra,Pavo,Sagitta

adverb
    always,inevitably,necessarily,surely,inescapably,assuredly

superlativeAdjective
    flawless,victorious,favoured,triumphant,successful,fortunate,propitious,lucky,outstanding,strong
    auspicious,crowned,extraordinary,unbeaten,undefeated,unconquered,prevailing,excellent,superior,greatest
    illustrious,splendid,fierce

genericAdjective
    blue,red,gray,bloody,yellow,black,white,azure
    mighty,great,straight,lightning

noun
    champion,challenger,defender,conqueror,guardian,paladin,vanquisher,victor,warrior,augury
    hammer,mallet,anvil
    sword,mercy
    blade,sabre,dagger,scimitar,foil,glaive
    arrow
    fury,anger,wrath,storm,lightning,thunder,omen,vengeance
    light,sunrise,peace
    Sun,Moon,Daystar
    cross,


potentiallyQualifiedNoun
    {noun}
    {noun}
    {noun} of {properNoun}
    {noun} of {properNoun}
    {noun} of the Gods

exalted
    {adverb} {superlativeAdjective}
    {adverb} {superlativeAdjective} {potentiallyQualifiedNoun}
    {superlativeAdjective} {potentiallyQualifiedNoun}

plain
    {noun}
    {genericAdjective} {properNoun}
    {genericAdjective} {noun}
    {superlativeAdjective}

ship
    {exalted}
    {plain}

fleet
    {genericAdjective} Fleet
"""


generatorByKey key =
    LexicalRandom.generator defaultLexicon key
    |> Random.map LexicalRandom.capitalize

ship =
    generatorByKey "ship"

fleet =
    generatorByKey "fleet"
