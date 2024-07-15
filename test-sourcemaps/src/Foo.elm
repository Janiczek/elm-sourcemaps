module Foo exposing (x)

import Bitwise


x : String
x =
    let
        repetitions =
            2 |> Bitwise.shiftLeftBy 3

        string =
            Char.fromCode 65
                |> String.fromChar
    in
    string
        |> String.repeat repetitions
