module CubeViewTest exposing (..)

import Array
import Cube exposing (..)
import CubeView exposing (colorsOfPosition)
import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Cube Test Suite"
        [ describe "キューブのポジションを指定すると色を返す"
            [ describe "ポジションが (1,-1,1) の場合は、top left front のみ"
                (let
                    pos =
                        ( 1, -1, 1 )
                 in
                 [ test "回転がない場合、順番に White Orange Green になる " <|
                    \_ ->
                        init ()
                            |> ofData
                            |> (\cube ->
                                    colorsOfPosition cube pos
                               )
                            |> Expect.equal
                                ([ Just White, Just Orange, Just Green, Nothing, Nothing, Nothing ] |> Array.fromList)
                 , test "Front を1回転した後だと、順番に Orange Yellow Green になる" <|
                    \_ ->
                        init ()
                            |> rotate Front
                            |> ofData
                            |> (\cube ->
                                    colorsOfPosition cube pos
                               )
                            |> Expect.equal
                                ([ Just Orange, Just Yellow, Just Green, Nothing, Nothing, Nothing ] |> Array.fromList)
                 , test "Left を1回転した後だと、順番に Blue Orange White になる" <|
                    \_ ->
                        init ()
                            |> rotate Left
                            |> ofData
                            |> (\cube ->
                                    colorsOfPosition cube pos
                               )
                            |> Expect.equal
                                ([ Just Blue, Just Orange, Just White, Nothing, Nothing, Nothing ] |> Array.fromList)
                 , test "Top を1回転した後だと、順番に White Green Red になる" <|
                    \_ ->
                        init ()
                            |> rotate Top
                            |> ofData
                            |> (\cube ->
                                    colorsOfPosition cube pos
                               )
                            |> Expect.equal
                                ([ Just White, Just Green, Just Red, Nothing, Nothing, Nothing ] |> Array.fromList)
                 , test "Front -> Front -> Left -> Left の順で回転した後だと、順番に Yellow Orange Blue になる" <|
                    \_ ->
                        init ()
                            |> (\data -> List.foldl rotate data [ Front, Front, Left, Left ])
                            |> ofData
                            |> (\cube ->
                                    colorsOfPosition cube pos
                               )
                            |> Expect.equal
                                ([ Just Yellow, Just Orange, Just Blue, Nothing, Nothing, Nothing ] |> Array.fromList)
                 ]
                )
            , describe "ポジションが (-1,1,-1) の場合は、right back down のみ"
                (let
                    pos =
                        ( -1, 1, -1 )
                 in
                 [ test "回転がない場合、順番に Red Blue Yellow  になる " <|
                    \_ ->
                        init ()
                            |> ofData
                            |> (\cube ->
                                    colorsOfPosition cube pos
                               )
                            |> Expect.equal
                                ([ Nothing, Nothing, Nothing, Just Red, Just Blue, Just Yellow ] |> Array.fromList)
                 ]
                )
            ]
        ]
