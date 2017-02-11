module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import List
import Array
import Sublist exposing (sublist)
import Last exposing (last)


split : List a -> Int -> ( List a, List a )
split list count =
    ( List.take count list, List.drop count list )


sortByListLengths : List (List Int) -> List Int
sortByListLengths xs =
    List.map List.length xs
        |> List.sort


sqrRoot : Int -> Int
sqrRoot num =
    num |> toFloat |> sqrt |> ceiling


isPrime : Int -> Bool
isPrime num =
    let
        range =
            List.range 2 (sqrRoot num)

        divisors =
            range
                |> List.filter (\x -> (num % x) /= 0)
    in
        -- Debug.log "boo-----" divisors
        List.length divisors == 0



-- penultimate : List a -> Maybe.Maybe a
-- penultimate xs =
--     let
--         arr =
--             Array.fromList xs
--     in
--         Array.get ((Array.length arr) - 2) arr


penultimate list =
    case List.drop 1 (List.reverse list) of
        [] ->
            Nothing

        y :: ys ->
            Just y


all : Test
all =
    describe "Sample Test Suite"
        [ describe "Unit test examples"
            [ test "Addition" <|
                \() ->
                    Expect.equal (sublist 3 7 (List.range 1 10)) [ 3, 4, 5, 6, 7 ]
            , test "String.left" <|
                \() ->
                    Expect.equal (sublist 3 -4 [ -3, -2, -1, 0, 1, 2, 3 ]) ([])
            , test "sortByListLengths" <|
                \() ->
                    Expect.equal (sortByListLengths [ [ 1 ], [ 3 ], [ 2, 4 ], [ 4, 1, 5 ], [ 5 ] ]) ([ 1, 1, 1, 2, 3 ])
            , test "sortByListLengths" <|
                \() ->
                    Expect.equal (sortByListLengths [ [ 1, 4, 5, 1 ], [ 3 ], [ 2, 4 ], [ 4, 1, 5 ], [ 5 ] ]) ([ 1, 1, 2, 3, 4 ])
            , test "isNotPrime" <|
                \() ->
                    Expect.equal (isPrime 64) False
            , test "isNotPrime" <|
                \() ->
                    Expect.equal (isPrime 144) False
            , test "Returns the last in a list" <|
                \() ->
                    Expect.equal (last [ 3, 2, 4, 5 ]) (Just (5))
            , test "Returns the last in a list" <|
                \() ->
                    Expect.equal (last [ 3, 2, 4, 5, 8 ]) (Just (8))
            , test "Returns the penultimate in a list" <|
                \() ->
                    Expect.equal (penultimate [ 3, 2, 4, 5, 8 ]) (Just (5))
              -- , test "isPrime" <|
              --     \() ->
              --         Expect.equal (isPrime 131) True
            ]
        ]
