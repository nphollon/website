module Main (main) where

import Html exposing (Html, main')
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
    main'
        []
        [ intro
        , Phys.view address model
        ]


intro : Html
intro =
    Markdown.toHtml """
I am a software developer with a background in physics & astronomy.

My latest project is a [classical mechanics library](http://package.elm-lang.org/packages/nphollon/mechanics/latest) for the [Elm programming language](http://elm-lang.org). Check here for updates and announcements!
"""


port tasks : Signal (Task Effects.Never ())
port tasks =
    app.tasks
