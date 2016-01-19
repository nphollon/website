module Orbits (Model, init, evolve, view) where

import Graphics.Collage as Collage
import Graphics.Element as Element exposing (Element)
import Color
import Dict exposing (Dict)
import Html exposing (..)
import Mechanics


type alias Model =
    StateWrapper


type alias StateWrapper =
    { state : Mechanics.State
    , keys : Dict String { position : Int, size : Int }
    }


initialize : Dict String (List ( Float, Float )) -> StateWrapper
initialize dataDict =
    let
        append key data { i, state, keys } =
            let
                size = List.length data
            in
                { i = i + size
                , state = state ++ data
                , keys = Dict.insert key { position = i, size = size } keys
                }

        flattened =
            Dict.foldl append { i = 0, state = [], keys = Dict.empty } dataDict
    in
        { state = Mechanics.state 0 flattened.state
        , keys = flattened.keys
        }


init : StateWrapper
init =
    Debug.log "init"
        <| orbital
        <| Dict.fromList
            [ ( "planetA", [ ( 0, 0 ), ( 0, 40 ) ] )
            , ( "planetB", [ ( -200, 0 ), ( 0, -400 ) ] )
            ]


masses : Dict String Float
masses =
    Dict.fromList [ ( "planetA", 50 ), ( "planetB", 5 ) ]


totalMass : Float
totalMass =
    Dict.foldl (\_ -> (+)) 0 masses


inverseMasses : Dict String Float
inverseMasses =
    Dict.map (\_ m -> totalMass - m) masses


orbital : Dict String (List ( Float, Float )) -> StateWrapper
orbital planets =
    let
        n = toFloat (Dict.size planets)

        subtract ( a, b ) ( c, d ) =
            ( a - c, b - d )

        weightAdd m ( x, v ) ( avgX, avgV ) =
            ( avgX + m * x / totalMass, avgV + m * v / totalMass )

        centerOfMass =
            Dict.foldl
                (\k ->
                    let
                        mass = Dict.get k masses |> Maybe.withDefault 0
                    in
                        List.map2 (weightAdd mass)
                )
                [ ( 0, 0 ), ( 0, 0 ) ]
                planets

        normalize coords =
            List.map2 subtract coords centerOfMass

        polarize coords =
            case List.take 2 coords of
                ( x, vx ) :: (( y, vy ) :: []) ->
                    let
                        ( r, phi ) = toPolar ( x, y )
                    in
                        [ ( r, (vx * x + vy * y) / r )
                        , ( phi, (vy * x - vx * y) / r ^ 2 )
                        ]

                _ ->
                    []
    in
        Dict.map (\k v -> polarize (normalize v)) planets
            |> Dict.insert "centerOfMass" centerOfMass
            |> initialize


coordinate : String -> Int -> StateWrapper -> Float
coordinate key index { state, keys } =
    case Dict.get key keys of
        Just { position, size } ->
            if index < size then
                Mechanics.coordinate (position + index) state
            else
                0

        Nothing ->
            0


velocity : String -> Int -> StateWrapper -> Float
velocity key index { state, keys } =
    case Dict.get key keys of
        Just { position, size } ->
            if index < size then
                Mechanics.velocity (position + index) state
            else
                0

        Nothing ->
            0


time : StateWrapper -> Float
time =
    .state >> Mechanics.time


evolve : Float -> Model -> Model
evolve dt model =
    let
        gravity = 500000.0

        accel key s =
            let
                invMass = Dict.get key inverseMasses |> Maybe.withDefault 1

                sw = { model | state = s }

                radius = coordinate key 0 sw

                radSpeed = velocity key 0 sw

                rotSpeed = velocity key 1 sw

                separation = radius * totalMass / invMass
            in
                [ (radius * rotSpeed ^ 2) - (2 * gravity * invMass * separation ^ -2)
                , -2 * radSpeed * rotSpeed / radius
                ]

        totalAccel s =
            [ 0, 0 ] ++ (accel "planetA" s) ++ (accel "planetB" s)
    in
        { model
            | state =
                Mechanics.evolve (Mechanics.acceleration totalAccel) dt model.state
        }


view : Model -> Html
view model =
    fromElement (draw model |> Element.color Color.white)


draw : Model -> Element
draw model =
    let
        planet key =
            let
                mass = sqrt (Dict.get key masses |> Maybe.withDefault 0) * 2

                radius = coordinate key 0 model

                angle = coordinate key 1 model

                radSpeed = velocity key 0 model

                rotSpeed = velocity key 1 model

                scale = 5
            in
                Collage.group
                    [ Collage.filled Color.blue (Collage.circle mass)
                    , Collage.segment
                        ( 0, 0 )
                        ( radSpeed / scale, radius * rotSpeed / scale )
                        |> Collage.traced (Collage.dotted Color.black)
                        |> Collage.rotate angle
                    ]
                    |> Collage.move (fromPolar ( radius, angle ))

        circle =
            Collage.circle >> Collage.outlined (Collage.dashed Color.grey)

        reference =
            Collage.group [ circle 50, circle 150, circle 250 ]
    in
        Collage.collage
            500
            500
            [ reference
            , planet "planetA"
            , planet "planetB"
            ]
