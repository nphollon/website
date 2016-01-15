module Main (main) where

import Html exposing (Html)
import Effects exposing (Effects)
import PhysicsFrame exposing (Model, init, update, view)
import StartApp exposing (App)
import Task exposing (Task)


app : App Model
app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = []
        }


main : Signal Html
main =
    app.html


port tasks : Signal (Task Effects.Never ())
port tasks =
    app.tasks


content : String
content =
    """
# Nick Hollon solves problems.

## Nick Hollon can solve _your_ problems.

### [ Get help today. ](mailto:contact@nickhollon.com)
"""
