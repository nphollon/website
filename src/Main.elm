module Main (main) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown
import Effects
import PhysicsFrame as Phys
import StartApp exposing (App)
import Task exposing (Task)


app : App Phys.Model
app =
    StartApp.start
        { init = Phys.init
        , update = Phys.update
        , view = view
        , inputs = []
        }


main : Signal Html
main =
    app.html


view : Signal.Address Phys.Action -> Phys.Model -> Html
view address model =
    figure (Phys.view address model) description


figure : Html -> String -> Html
figure content label =
    article
        [ class "figure" ]
        [ content
        , div [ class "figure-label" ] [ Markdown.toHtml label ]
        ]


description : String
description =
    """
A moon orbits a planet ten times its mass. The solid gray lines represent the velocities of the two objects (the longer the line, the faster the object is moving). The dashed circles are for reference so that you can see that the orbits are ellipses, not circles.
"""


port tasks : Signal (Task Effects.Never ())
port tasks =
    app.tasks
