module Orbits (Model, init, evolve, view) where

import Graphics.Collage as Collage
import Color
import Dict exposing (Dict)
import Html exposing (..)
import Mechanics


type alias Model =
    StateWrapper


type alias StateWrapper =
    { state : Mechanics.State
    , indexes : Dict String Int
    , masses : Dict String Float
    }


type alias PointData =
    { position : Vector
    , velocity : Vector
    , mass : Float
    }


type alias Vector =
    { x : Float, y : Float }


vector : Float -> Float -> Vector
vector x y =
    { x = x, y = y }


initialize : Dict String PointData -> StateWrapper
initialize dataDict =
    let
        toList { position, velocity } =
            [ ( position.x, velocity.x ), ( position.y, velocity.y ) ]

        append key data { i, state, indexes } =
            { i = i + 2
            , state = state ++ toList data
            , indexes = Dict.insert key i indexes
            }

        flattened =
            Dict.foldl append { i = 0, state = [], indexes = Dict.empty } dataDict
    in
        { state = Mechanics.state 0 flattened.state
        , indexes = flattened.indexes
        , masses = Dict.map (\_ -> .mass) dataDict
        }


coordinate : String -> Int -> StateWrapper -> Float
coordinate key i { state, indexes } =
    case Dict.get key indexes of
        Just offset ->
            if i < 2 then
                Mechanics.coordinate (offset + i) state
            else
                0

        Nothing ->
            0


velocity : String -> Int -> StateWrapper -> Float
velocity key i { state, indexes } =
    case Dict.get key indexes of
        Just offset ->
            if i < 2 then
                Mechanics.velocity (offset + i) state
            else
                0

        Nothing ->
            0


mass : String -> StateWrapper -> Float
mass key { masses } =
    Dict.get key masses |> Maybe.withDefault 0


totalMass : StateWrapper -> Float
totalMass =
    .masses >> Dict.foldl (\_ -> (+)) 0


time : StateWrapper -> Float
time =
    .state >> Mechanics.time


centerOfMass : List PointData -> PointData
centerOfMass planets =
    let
        weightedSum planet center =
            { mass =
                center.mass + planet.mass
            , position =
                add center.position (scale planet.mass planet.position)
            , velocity =
                add center.velocity (scale planet.mass planet.velocity)
            }

        divideByMass center =
            { center
                | position = scale (1 / center.mass) center.position
                , velocity = scale (1 / center.mass) center.velocity
            }

        defaultCenter =
            { mass = 0, position = vector 0 0, velocity = vector 0 0 }
    in
        List.foldl weightedSum defaultCenter planets
            |> divideByMass


add : Vector -> Vector -> Vector
add a b =
    { x = a.x + b.x
    , y = a.y + b.y
    }


subtract : Vector -> Vector -> Vector
subtract a b =
    { x = a.x - b.x
    , y = a.y - b.y
    }


scale : Float -> Vector -> Vector
scale c v =
    { x = v.x * c
    , y = v.y * c
    }


recenter : PointData -> PointData -> PointData
recenter origin object =
    { object
        | position = subtract object.position origin.position
        , velocity = subtract object.velocity origin.velocity
    }


polarize : PointData -> PointData
polarize { position, velocity, mass } =
    let
        ( r, phi ) = toPolar ( position.x, position.y )
    in
        { position = { x = r, y = phi }
        , velocity =
            { x = (velocity.x * position.x + velocity.y * position.y) / r
            , y = (velocity.y * position.x - velocity.x * position.y) / r ^ 2
            }
        , mass = mass
        }


type alias Rules =
    Dict String (StateWrapper -> Vector)



------------------------------------------------------------------


init : StateWrapper
init =
    Debug.log "init"
        <| orbital
        <| Dict.fromList
            [ ( "planetA"
              , { position = vector 0 0
                , velocity = vector 0 40
                , mass = 50
                }
              )
            , ( "planetB"
              , { position = vector -200 0
                , velocity = vector 0 -400
                , mass = 5
                }
              )
            ]


orbital : Dict String PointData -> StateWrapper
orbital planets =
    let
        origin = centerOfMass (Dict.values planets)
    in
        Dict.map (\_ -> recenter origin >> polarize) planets
            |> initialize


evolve : Float -> Model -> Model
evolve dt model =
    let
        gravity = 500000.0

        accel key s =
            let
                totMass = totalMass model

                invMass = totMass - (mass key model)

                sw = { model | state = s }

                radius = coordinate key 0 sw

                radSpeed = velocity key 0 sw

                rotSpeed = velocity key 1 sw

                separation = radius * totMass / invMass
            in
                [ (radius * rotSpeed ^ 2) - (2 * gravity * invMass * separation ^ -2)
                , -2 * radSpeed * rotSpeed / radius
                ]

        totalAccel s =
            (accel "planetA" s) ++ (accel "planetB" s)
    in
        { model
            | state =
                Mechanics.evolve (Mechanics.acceleration totalAccel) dt model.state
        }


view : Model -> Html
view model =
    let
        planet key =
            let
                m = sqrt (mass key model)

                rScale = 0.8

                vScale = 0.15

                radius = rScale * coordinate key 0 model

                angle = coordinate key 1 model

                radSpeed = vScale * velocity key 0 model

                rotSpeed = vScale * velocity key 1 model
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
