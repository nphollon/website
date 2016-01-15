module Main (..) where

import Html exposing (..)
import Markdown


main : Html
main =
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
