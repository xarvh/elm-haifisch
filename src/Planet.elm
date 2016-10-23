module Planet exposing (..)

import Common exposing (..)
import Random
import Time


randomAngle =
    Random.float (turns -0.5) (turns 0.5)


wr =
    Random.map ((*) worldRadius)


wrS =
    Random.map ((*) (worldRadius / Time.second))


satelliteGenerator : Random.Generator Satellite
satelliteGenerator =
    Random.map3 Satellite
        (wr <| Random.float 0.01 0.05)
        (wrS <| Random.float 0.04 0.15)
        randomAngle


angularSpeed : Float -> Float
angularSpeed orbitRadius =
    -- https://en.wikipedia.org/wiki/Kepler%27s_laws_of_planetary_motion#Third_law
    (0.25 / Time.second) * turns 1 * (orbitRadius / worldRadius) ^ (-1.5)


makePlanet orbitRadius =
    Planet orbitRadius (angularSpeed orbitRadius)


planetGenerator : Random.Generator Planet
planetGenerator =
    Random.map4 makePlanet
        (wr <| Random.float 0.1 0.9)
        (wrS <| Random.float 0.003 0.009)
        randomAngle
    <|
        Random.int 0 4
            `Random.andThen`
                \numberOfSatellites ->
                    Random.list numberOfSatellites satelliteGenerator


planetsGenerator : Random.Generator (List Planet)
planetsGenerator =
    Random.int 2 4 `Random.andThen` \numberOfPlanets ->
        Random.list numberOfPlanets planetGenerator
