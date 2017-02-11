module Sublist exposing (..)


sublist : Int -> Int -> List a -> List a
sublist start end list =
    let
        normalStart =
            if start < 1 then
                1
            else
                start
    in
        list
            |> List.drop (normalStart - 1)
            |> List.take (end - (normalStart - 1))
