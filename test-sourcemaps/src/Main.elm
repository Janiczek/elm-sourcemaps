module Main exposing (main)

import Foo
import Html


main =
    Foo.x
        |> Debug.toString
        |> Html.text
