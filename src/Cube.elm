module Cube exposing (Color(..), Cube, Data, init, ofColor, ofData, sideOf)

import Array exposing (Array)
import Color as ObjColor


type Color
    = White
    | Orange
    | Green
    | Red
    | Blue
    | Yellow


numberOfColor : Color -> Int
numberOfColor color =
    case color of
        White ->
            0

        Yellow ->
            1

        Green ->
            2

        Blue ->
            3

        Orange ->
            4

        Red ->
            5


ofColor : Color -> ObjColor.Color
ofColor color =
    case color of
        White ->
            ObjColor.white

        Orange ->
            ObjColor.orange

        Green ->
            ObjColor.green

        Red ->
            ObjColor.red

        Blue ->
            ObjColor.blue

        Yellow ->
            ObjColor.yellow


type CornerOrientation
    = NormalRotate
    | Right
    | Left


rotate : CornerOrientation -> ( Color, Color, Color ) -> ( Color, Color, Color )
rotate co ( c1, c2, c3 ) =
    case co of
        NormalRotate ->
            ( c1, c2, c3 )

        Right ->
            ( c3, c1, c2 )

        Left ->
            ( c2, c3, c1 )


type EdgeOrientation
    = Normal
    | Reversed


turn : EdgeOrientation -> ( Color, Color ) -> ( Color, Color )
turn eo ( c1, c2 ) =
    case eo of
        Normal ->
            ( c1, c2 )

        Reversed ->
            ( c2, c1 )


type alias Data =
    { corner : Array ( Int, CornerOrientation )
    , edge : Array ( Int, EdgeOrientation )
    }


corner : Data -> Int -> ( Color, Color, Color )
corner data n =
    let
        ( block, rot ) =
            Array.get n data.corner |> Maybe.withDefault ( -1, NormalRotate )

        colors =
            case block of
                0 ->
                    ( White, Orange, Blue )

                1 ->
                    ( White, Blue, Red )

                2 ->
                    ( White, Red, Green )

                3 ->
                    ( White, Green, Orange )

                4 ->
                    ( Yellow, Blue, Orange )

                5 ->
                    ( Yellow, Red, Blue )

                6 ->
                    ( Yellow, Green, Red )

                7 ->
                    ( Yellow, Orange, Green )

                _ ->
                    ( White, White, White )
    in
    rotate rot colors


edge : Data -> Int -> ( Color, Color )
edge data n =
    let
        ( block, rot ) =
            Array.get n data.edge |> Maybe.withDefault ( -1, Normal )

        colors =
            case block of
                0 ->
                    ( White, Blue )

                1 ->
                    ( White, Orange )

                2 ->
                    ( White, Green )

                3 ->
                    ( White, Red )

                4 ->
                    ( Yellow, Blue )

                5 ->
                    ( Yellow, Orange )

                6 ->
                    ( Yellow, Green )

                7 ->
                    ( Yellow, Red )

                8 ->
                    ( Green, Orange )

                9 ->
                    ( Green, Red )

                10 ->
                    ( Blue, Red )

                11 ->
                    ( Blue, Orange )

                _ ->
                    ( White, White )
    in
    turn rot colors


init : () -> Data
init _ =
    Data
        (Array.fromList <| List.map (\i -> ( i, NormalRotate )) <| List.range 0 7)
        (Array.fromList <| List.map (\i -> ( i, Normal )) <| List.range 0 11)


type alias Cube =
    Array Color


initCube : () -> Cube
initCube _ =
    List.indexedMap (\i c -> ( i, c )) [ White, Orange, Green, Red, Blue, Yellow ]
        |> List.foldl
            (\( i, color ) cub -> Array.set (indexOfPosition ( i, 4 )) color cub)
            (Array.repeat 54 White)


sideOf : Cube -> Int -> Array Color
sideOf cube i =
    Array.slice (i * 9) ((i + 1) * 9) cube


getColor : Cube -> Position -> Maybe Color
getColor cube pos =
    Array.get (indexOfPosition pos) cube


setColor : Position -> Color -> Cube -> Cube
setColor pos color cube =
    Array.set (indexOfPosition pos) color cube


type alias Position =
    ( Int, Int )


indexOfPosition : Position -> Int
indexOfPosition ( p1, p2 ) =
    p1 * 9 + p2


cornerPosition : Int -> ( Position, Position, Position )
cornerPosition n =
    case n of
        0 ->
            ( ( 0, 0 ), ( 1, 0 ), ( 4, 2 ) )

        1 ->
            ( ( 0, 2 ), ( 4, 0 ), ( 3, 2 ) )

        2 ->
            ( ( 0, 8 ), ( 3, 0 ), ( 2, 2 ) )

        3 ->
            ( ( 0, 6 ), ( 2, 0 ), ( 1, 2 ) )

        4 ->
            ( ( 5, 6 ), ( 4, 8 ), ( 1, 6 ) )

        5 ->
            ( ( 5, 8 ), ( 3, 8 ), ( 4, 6 ) )

        6 ->
            ( ( 5, 2 ), ( 2, 8 ), ( 3, 6 ) )

        7 ->
            ( ( 5, 0 ), ( 1, 8 ), ( 2, 6 ) )

        _ ->
            ( ( -1, -1 ), ( -1, -1 ), ( -1, -1 ) )


edgePosition : Int -> ( Position, Position )
edgePosition n =
    case n of
        0 ->
            ( ( 0, 1 ), ( 4, 1 ) )

        1 ->
            ( ( 0, 3 ), ( 1, 1 ) )

        2 ->
            ( ( 0, 7 ), ( 2, 1 ) )

        3 ->
            ( ( 0, 5 ), ( 3, 1 ) )

        4 ->
            ( ( 5, 7 ), ( 4, 7 ) )

        5 ->
            ( ( 5, 3 ), ( 1, 7 ) )

        6 ->
            ( ( 5, 1 ), ( 2, 7 ) )

        7 ->
            ( ( 5, 5 ), ( 3, 7 ) )

        8 ->
            ( ( 2, 3 ), ( 1, 5 ) )

        9 ->
            ( ( 2, 5 ), ( 3, 3 ) )

        10 ->
            ( ( 4, 3 ), ( 3, 5 ) )

        11 ->
            ( ( 4, 5 ), ( 1, 3 ) )

        _ ->
            ( ( -1, -1 ), ( -1, -1 ) )


setCornerColor : Data -> Int -> Cube -> Cube
setCornerColor data i cube =
    let
        ( c1, c2, c3 ) =
            corner data i

        ( p1, p2, p3 ) =
            cornerPosition i
    in
    [ ( c1, p1 ), ( c2, p2 ), ( c3, p3 ) ]
        |> List.foldl (\( c, p ) cub -> Array.set (indexOfPosition p) c cub) cube


setEdgeColor : Data -> Int -> Cube -> Cube
setEdgeColor data i cube =
    let
        ( c1, c2 ) =
            edge data i

        ( p1, p2 ) =
            edgePosition i
    in
    [ ( c1, p1 ), ( c2, p2 ) ]
        |> List.foldl (\( c, p ) cub -> Array.set (indexOfPosition p) c cub) cube


ofData : Data -> Cube
ofData data =
    let
        setCornerColors cube =
            List.foldl
                (\i cub -> setCornerColor data i cub)
                cube
                (List.range 0 7)

        setEdgeColors cube =
            List.foldl
                (\i cub -> setEdgeColor data i cub)
                cube
                (List.range 0 11)
    in
    initCube ()
        |> setCornerColors
        |> setEdgeColors
