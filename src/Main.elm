module Main (..) where

import Html exposing (..)
import StartApp
import Signal
import Markdown
import Effects exposing (Effects)


main : Signal Html
main =
    .html
        <| StartApp.start
            { init = ( init, Effects.none )
            , update = update
            , view = view
            , inputs = []
            }


type alias Action =
    {}


type alias Model =
    {}


init : Model
init =
    {}


update : Action -> Model -> ( Model, Effects Action )
update _ _ =
    ( init, Effects.none )


view : Signal.Address Action -> Model -> Html
view _ _ =
    main'
        []
        [ Markdown.toHtml content
        ]


content : String
content =
    """
# Nick Hollon solves problems.

## Nick Hollon can solve _your_ problems.

### [ Get help today. ](mailto:contact@nickhollon.com)
"""
