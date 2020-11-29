module CubeTest exposing (..)

import Array
import Cube exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "Cube Test Suite"
        [ test "初期のキューブ" <|
            \_ ->
                init ()
                    |> ofData
                    |> Expect.equal
                        (List.map (List.repeat 9) [ White, Orange, Green, Red, Blue, Yellow ] |> List.concat |> Array.fromList)
        ]
