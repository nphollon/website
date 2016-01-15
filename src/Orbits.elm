module Orbits (Model, init, evolve, view) where

import Graphics.Collage as Collage
import Color
import Html exposing (..)
import Mechanics


type alias Model =
    Mechanics.State


init : Model
init =
    Mechanics.state1 ( 0, -500 )


evolve : Float -> Model -> Model
evolve dt model =
    let
        spring =
            Mechanics.acceleration
                <| \s -> [ -25 * (Mechanics.coordinate 0 s) ]
    in
        Mechanics.evolve spring dt model


view : Model -> Html
view model =
    fromElement (Collage.collage 500 100 [ draw model ])


draw : Mechanics.State -> Collage.Form
draw state =
    Collage.circle 20
        |> Collage.filled Color.blue
        |> Collage.moveX (Mechanics.coordinate 0 state)
