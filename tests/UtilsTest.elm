module UtilsTest exposing (..)

import Expect
import Math.Vector2 exposing (vec2)
import Test exposing (..)
import Utils exposing (intersection)


suite : Test
suite =
    describe "Utils Test Suite"
        [ describe "intersection"
            [ describe "直線が交差しないとき Nothing になる"
                [ test "平行な場合" <|
                    \_ ->
                        intersection
                            { p1 = vec2 0 0, p2 = vec2 1 1 }
                            { p1 = vec2 1 0, p2 = vec2 3 2 }
                            |> Expect.equal Nothing
                ]
            , describe "交差するとき、交点を返す"
                [ test "y = 2x - 3 と y = -x + 3 の交点は (x,y) = (2,1)" <|
                    \_ ->
                        intersection
                            { p1 = vec2 0 -3, p2 = vec2 1 -1 }
                            { p1 = vec2 0 3, p2 = vec2 2 1 }
                            |> Expect.equal (Just (vec2 2 1))
                , test "y = 3x - 1 と y = 2x + 5 の交点は (x,y) = (6,17)" <|
                    \_ ->
                        intersection
                            { p1 = vec2 0 -1, p2 = vec2 1 2 }
                            { p1 = vec2 0 5, p2 = vec2 1 7 }
                            |> Expect.equal (Just (vec2 6 17))
                ]
            ]
        ]
