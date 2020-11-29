module Cube exposing (Color(..), Cube, Data, init, ofColor, ofData, sideOf)

import Array exposing (Array)
import Color as ObjColor
import Result exposing (fromMaybe)


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
    | RightRotate
    | LeftRotate


addCornerOrientation : CornerOrientation -> CornerOrientation -> CornerOrientation
addCornerOrientation o1 o2 =
    case ( o1, o2 ) of
        ( NormalRotate, _ ) ->
            o2

        ( _, NormalRotate ) ->
            o1

        ( LeftRotate, LeftRotate ) ->
            RightRotate

        ( RightRotate, RightRotate ) ->
            LeftRotate

        _ ->
            NormalRotate


rotateCorner : CornerOrientation -> ( Color, Color, Color ) -> ( Color, Color, Color )
rotateCorner co ( c1, c2, c3 ) =
    case co of
        NormalRotate ->
            ( c1, c2, c3 )

        RightRotate ->
            ( c3, c1, c2 )

        LeftRotate ->
            ( c2, c3, c1 )


type EdgeOrientation
    = Normal
    | Reversed


addEdgeOrientation : EdgeOrientation -> EdgeOrientation -> EdgeOrientation
addEdgeOrientation e1 e2 =
    if e1 == e2 then
        Normal

    else
        Reversed


turnEdge : EdgeOrientation -> ( Color, Color ) -> ( Color, Color )
turnEdge eo ( c1, c2 ) =
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
    rotateCorner rot colors


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
    turnEdge rot colors


type Side
    = Top
    | Left
    | Front
    | Right
    | Back
    | Down


rotate : Side -> Data -> Data
rotate side data =
    let
        ( cornerPermutation, edgePermutation ) =
            case side of
                Top ->
                    ( [ ( 0, 3, NormalRotate ), ( 1, 0, NormalRotate ), ( 2, 1, NormalRotate ), ( 3, 2, NormalRotate ) ]
                    , [ ( 0, 1, Normal ), ( 1, 2, Normal ), ( 2, 3, Normal ), ( 3, 0, Normal ) ]
                    )

                Left ->
                    ( [ ( 0, 4, LeftRotate ), ( 3, 0, RightRotate ), ( 7, 3, LeftRotate ), ( 4, 7, RightRotate ) ]
                    , [ ( 1, 11, Normal ), ( 8, 1, Normal ), ( 5, 8, Normal ), ( 11, 5, Normal ) ]
                    )

                Front ->
                    ( [ ( 3, 7, LeftRotate ), ( 2, 3, RightRotate ), ( 6, 2, LeftRotate ), ( 7, 6, RightRotate ) ]
                    , [ ( 2, 8, Reversed ), ( 9, 2, Reversed ), ( 6, 9, Reversed ), ( 8, 6, Reversed ) ]
                    )

                Right ->
                    ( [ ( 1, 2, RightRotate ), ( 2, 6, LeftRotate ), ( 6, 5, RightRotate ), ( 5, 1, LeftRotate ) ]
                    , [ ( 3, 9, Normal ), ( 10, 3, Normal ), ( 7, 10, Normal ), ( 9, 7, Normal ) ]
                    )

                Back ->
                    ( [ ( 0, 1, RightRotate ), ( 1, 5, LeftRotate ), ( 5, 4, RightRotate ), ( 4, 0, LeftRotate ) ]
                    , [ ( 11, 0, Reversed ), ( 4, 11, Reversed ), ( 10, 4, Reversed ), ( 0, 10, Reversed ) ]
                    )

                Down ->
                    ( [ ( 7, 4, NormalRotate ), ( 6, 7, NormalRotate ), ( 5, 6, NormalRotate ), ( 4, 5, NormalRotate ) ]
                    , [ ( 6, 5, Normal ), ( 7, 6, Normal ), ( 4, 7, Normal ), ( 5, 4, Normal ) ]
                    )

        nextCorner =
            cornerPermutation
                |> List.foldl
                    (\( idx, next, rot1 ) co ->
                        Array.set idx
                            (Array.get next data.corner
                                |> Maybe.withDefault ( 0, NormalRotate )
                                |> (\( v, rot2 ) -> ( v, addCornerOrientation rot1 rot2 ))
                            )
                            co
                    )
                    data.corner

        nextEdge =
            edgePermutation
                |> List.foldl
                    (\( idx, next, turn1 ) eo ->
                        Array.set idx
                            (Array.get next data.edge
                                |> Maybe.withDefault ( 0, Normal )
                                |> (\( v, turn2 ) -> ( v, addEdgeOrientation turn1 turn2 ))
                            )
                            eo
                    )
                    data.edge
    in
    { data | corner = nextCorner, edge = nextEdge }


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
