module CubeViewTest exposing (..)

import Cube exposing (..)
import CubeView exposing (CubeColors, colorsOfPosition, ofCube)
import Expect
import Test exposing (..)


assertColorsOfPosition : CubeColors -> ( Int, Int, Int ) -> List (Maybe Color) -> () -> Expect.Expectation
assertColorsOfPosition cube pos expectedColors () =
    colorsOfPosition cube pos
        |> Expect.equal expectedColors


rotatedCube : List Side -> CubeColors
rotatedCube sides =
    init ()
        |> (\cube -> List.foldl (\side -> rotate side CW) cube sides)
        |> ofCube


suite : Test
suite =
    describe "Cube Test Suite"
        [ describe "キューブのポジションを指定すると色を返す"
            [ describe "キューブを回転していないとき"
                (let
                    cube =
                        rotatedCube []

                    assertion =
                        assertColorsOfPosition cube
                 in
                 [ describe "ポジションが (1,-1,1) の場合は、top left front のみ"
                    [ test "順番に White Orange Green になる" <|
                        assertion ( 1, -1, 1 ) [ Just White, Just Orange, Just Green, Nothing, Nothing, Nothing ]
                    ]
                 , describe "ポジションが (-1,1,-1) の場合は、right back down のみ"
                    [ test "順番に Red Blue Yellow  になる " <|
                        assertion ( -1, 1, -1 ) [ Nothing, Nothing, Nothing, Just Red, Just Blue, Just Yellow ]
                    ]
                 , describe "ポジションが (0, 1, 1) の場合は、 top right のみ"
                    [ test "順番に White Red になる" <|
                        assertion ( 0, 1, 1 ) [ Just White, Nothing, Nothing, Just Red, Nothing, Nothing ]
                    ]
                 , describe "ポジションが (-1, -1, 0) の場合は、 left back のみ"
                    [ test "順番に Orange Blue になる" <|
                        assertion ( -1, -1, 0 ) [ Nothing, Just Orange, Nothing, Nothing, Just Blue, Nothing ]
                    ]
                 , describe "ポジションが (1, 0, 0) の場合は、 front のみ"
                    [ test "順番に Green になる" <|
                        assertion ( 1, 0, 0 ) [ Nothing, Nothing, Just Green, Nothing, Nothing, Nothing ]
                    ]
                 ]
                )
            , describe "Top -> Front の順で回転したとき"
                (let
                    cube =
                        rotatedCube [ Top, Front ]

                    assertion =
                        assertColorsOfPosition cube
                 in
                 [ describe "ポジションが (1,-1,1) の場合は、top left front のみ"
                    [ test "順番に Orange Yellow Green になる" <|
                        assertion ( 1, -1, 1 ) [ Just Orange, Just Yellow, Just Green, Nothing, Nothing, Nothing ]
                    ]
                 , describe "ポジションが (-1,1,-1) の場合は、right back down のみ"
                    [ test "順番に Red Blue Yellow  になる " <|
                        assertion ( -1, 1, -1 ) [ Nothing, Nothing, Nothing, Just Red, Just Blue, Just Yellow ]
                    ]
                 , describe "ポジションが (0, 1, 1) の場合は、 top right のみ"
                    [ test "順番に White Blue になる" <|
                        assertion ( 0, 1, 1 ) [ Just White, Nothing, Nothing, Just Blue, Nothing, Nothing ]
                    ]
                 , describe "ポジションが (-1, -1, 0) の場合は、 left back のみ"
                    [ test "順番に Orange Blue になる" <|
                        assertion ( -1, -1, 0 ) [ Nothing, Just Orange, Nothing, Nothing, Just Blue, Nothing ]
                    ]
                 , describe "ポジションが (1, 0, 0) の場合は、 front のみ"
                    [ test "順番に Green になる" <|
                        assertion ( 1, 0, 0 ) [ Nothing, Nothing, Just Green, Nothing, Nothing, Nothing ]
                    ]
                 ]
                )
            ]
        ]
