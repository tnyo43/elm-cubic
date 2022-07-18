module CubeTest exposing (..)

import Cube exposing (..)
import Expect
import Test exposing (..)


rotateNTimes : Side -> Direction -> Int -> Cube -> Cube
rotateNTimes side direction =
    let
        sub n c =
            if n == 0 then
                c

            else
                rotateSide side direction c |> sub (n - 1)
    in
    sub


suite : Test
suite =
    describe "Cube Test Suite"
        [ describe "キューブの順方向に3回回転すると、逆方向に1回回転したときと一致する" <|
            let
                cube =
                    Cube.init ()
            in
            [ test "Top" <|
                \_ -> Expect.equal (rotateNTimes Top CW 3 cube) (rotateNTimes Top CCW 1 cube)
            , test "Left" <|
                \_ -> Expect.equal (rotateNTimes Left CW 3 cube) (rotateNTimes Left CCW 1 cube)
            , test "Front" <|
                \_ -> Expect.equal (rotateNTimes Front CW 3 cube) (rotateNTimes Front CCW 1 cube)
            , test "Right" <|
                \_ -> Expect.equal (rotateNTimes Right CW 3 cube) (rotateNTimes Right CCW 1 cube)
            , test "Back" <|
                \_ -> Expect.equal (rotateNTimes Back CW 3 cube) (rotateNTimes Back CCW 1 cube)
            , test "Bottom" <|
                \_ -> Expect.equal (rotateNTimes Bottom CW 3 cube) (rotateNTimes Bottom CCW 1 cube)
            ]
        ]
