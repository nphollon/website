module Orbits (Model, init, evolve, view) where

import Graphics.Collage as Collage
import Color
import Dict exposing (Dict)
import Html exposing (..)
import Mechanics


type alias Model =
    Mechanics.State


init : Mechanics.State
init =
    Debug.log "init"
        <| orbital
        <| Dict.fromList
            [ ( "planetA"
              , { position = Mechanics.vector 0 0
                , velocity = Mechanics.vector 0 40
                , mass = 50
                }
              )
            , ( "planetB"
              , { position = Mechanics.vector -200 0
                , velocity = Mechanics.vector 0 -400
                , mass = 5
                }
              )
            ]


orbital : Dict String Mechanics.Particle -> Mechanics.State
orbital planets =
    let
        origin = Mechanics.centerOfMass (Dict.values planets)
    in
        Dict.map (\_ -> Mechanics.recenter origin >> Mechanics.polarize) planets
            |> Mechanics.initialize


evolve : Float -> Model -> Model
evolve dt model =
    let
        gravity = 500000.0

        totMass = Mechanics.totalMass model

        accel particle s =
            let
                invMass = totMass - particle.mass

                radius = particle.position.x

                radSpeed = particle.velocity.x

                rotSpeed = particle.velocity.y

                separation = radius * totMass / invMass
            in
                Mechanics.vector
                    ((radius * rotSpeed ^ 2) - (2 * gravity * invMass * separation ^ -2))
                    (-2 * radSpeed * rotSpeed / radius)

        acceleration =
            Dict.fromList
                [ ( "planetA", accel )
                , ( "planetB", accel )
                ]
    in
        Mechanics.evolve acceleration dt model


view : Model -> Html
view model =
    let
        planet key =
            case Mechanics.particle key model of
                Just particle ->
                    let
                        m = sqrt particle.mass

                        rScale = 0.8

                        vScale = 0.15

                        radius = rScale * particle.position.x

                        angle = particle.position.y

                        radSpeed = vScale * particle.velocity.x

                        rotSpeed = vScale * particle.velocity.y
                    in
                        Collage.group
                            [ Collage.filled Color.blue (Collage.circle m)
                            , Collage.segment
                                ( 0, 0 )
                                ( radSpeed, radius * rotSpeed )
                                |> Collage.traced (Collage.solid Color.darkGrey)
                                |> Collage.rotate angle
                            ]
                            |> Collage.move (fromPolar ( radius, angle ))

                Nothing ->
                    Collage.group []

        circle =
            Collage.circle >> Collage.outlined (Collage.dashed Color.grey)

        reference =
            Collage.group [ circle 30, circle 90, circle 150 ]
    in
        Collage.collage
            400
            400
            [ reference
            , planet "planetA"
            , planet "planetB"
            ]
            |> Html.fromElement
