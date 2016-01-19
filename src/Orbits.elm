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
            [ ( "planetA", [ ( 100, 0 ), ( 50, 200 ) ] )
            , ( "planetB", [ ( -100, 20 ), ( 0, -200 ) ] )
            ]


orbital : Dict String (List ( Float, Float )) -> StateWrapper
orbital planets =
    let
        subtract ( a, b ) ( c, d ) =
            ( a - c, b - d )

        divideBy c ( a, b ) =
            ( a / c, b / c )

        add ( a, b ) ( c, d ) =
            ( a + c, b + d )

        centerOfMass =
            Dict.foldl (\k -> List.map2 add) [ ( 0, 0 ), ( 0, 0 ) ] planets
                |> List.map (divideBy (toFloat (Dict.size planets)))

        normalize coords =
            List.map2
                (\( x, v ) ( x0, v0 ) -> ( x - x0, v - v0 ))
                coords
                centerOfMass

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
        -- assumes two planets are of the same mass
        gravity = 5000000.0

        accel key s =
            let
                sw = { model | state = s }

                radius = coordinate key 0 sw

                radSpeed = velocity key 0 sw

                rotSpeed = velocity key 1 sw
            in
                [ (radius * rotSpeed ^ 2) - (gravity * radius ^ -2)
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
        ball key =
            let
                radius = coordinate key 0 model

                angle = coordinate key 1 model
            in
                Collage.circle 10
                    |> Collage.filled Color.blue
                    |> Collage.move (fromPolar ( radius, angle ))

        circle =
            Collage.circle >> Collage.outlined (Collage.dashed Color.grey)

        reference key =
            let
                x = coordinate key 0 model

                y = coordinate key 1 model
            in
                Collage.group [ circle 2, circle 100, circle 300 ]
                    |> Collage.move ( -x, -y )
    in
        Collage.collage
            500
            500
            [ reference "centerOfMass"
            , ball "planetA"
            , ball "planetB"
            ]
