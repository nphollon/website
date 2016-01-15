module Main (..) where

import Html exposing (..)
import Html.Attributes exposing (href)


main : Html
main =
    main'
        []
        [ h1 [] [ text "Nick Hollon solves problems" ]
        , h2 [] [ text "Nick Hollon can solve <em>your</em> problems" ]
        , h3 [] [ a [ href "mailto:contact@nickhollon.com" ] [ text "Get help today" ] ]
        ]
