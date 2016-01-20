module Main (main) where

import Html exposing (..)
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
        [ header [] [ text "Nick Hollon" ]
        , article [] [ intro ]
        , article
            []
            [ firstEntry
            , Phys.view address model
            ]
        ]


intro : Html
intro =
    Markdown.toHtml """
I am a software developer with a background in physics & astronomy.

My latest project is a [classical mechanics library](http://package.elm-lang.org/packages/nphollon/mechanics/latest) for the [Elm programming language](http://elm-lang.org). Check here for updates and announcements!

You can email me  [here](mailto:contact@nickhollon.com).
"""


firstEntry : Html
firstEntry =
    Markdown.toHtml """
# January 19, 2016

Here is an example built using _nphollon/mechanics_. A moon orbits a planet ten times its mass. The solid gray lines represent the velocities of the two objects (the longer the line, the faster the object is moving). The dashed circles are just for reference so that you can see that the orbit is not circular.

This is the first example I've built since updating the library to version 3. The library is extremely low-level at this point (I had to compute the accelerations by hand). Writing this gave me a lot of ideas for making it both more usable and more powerful.
"""


port tasks : Signal (Task Effects.Never ())
port tasks =
    app.tasks
