module PhysicsFrame (Model, Action, init, update, view) where

import Html exposing (..)
import Html.Events exposing (onClick)
import Effects exposing (Effects)
import Time exposing (Time)
import Orbits


type alias Model =
    { state : Orbits.Model
    , prevClockTime : Maybe Time
    , isPaused : Bool
    }


init : ( Model, Effects Action )
init =
    (,)
        { state = Orbits.init
        , prevClockTime = Nothing
        , isPaused = True
        }
        Effects.none


type Action
    = Tick Time
    | Start
    | Pause
    | Reset


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        Tick t ->
            if model.isPaused then
                ( model, Effects.none )
            else
                ( tick t model, Effects.tick Tick )

        Start ->
            ( { model | isPaused = False }, Effects.tick Tick )

        Pause ->
            ( { model | isPaused = True, prevClockTime = Nothing }, Effects.none )

        Reset ->
            init


tick : Time -> Model -> Model
tick clockTime model =
    let
        withUpdatedTime =
            { model
                | prevClockTime = Just clockTime
            }
    in
        case model.prevClockTime of
            Just prevClockTime ->
                let
                    dt = Time.inSeconds (clockTime - prevClockTime)
                in
                    { withUpdatedTime
                        | state = Orbits.evolve dt model.state
                    }

            Nothing ->
                withUpdatedTime


view : Signal.Address Action -> Model -> Html
view address model =
    div
        []
        [ Orbits.view model.state
        , button [ onClick address Start ] [ text "Play" ]
        , button [ onClick address Pause ] [ text "Pause" ]
        , button [ onClick address Reset ] [ text "Reset" ]
        ]
