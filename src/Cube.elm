module Cube exposing (Axis(..), Color(..), CornerOrientation(..), Cube, Direction(..), EdgeOrientation(..), Side(..), init, rotateCorner, rotateMiddle, rotateSide, sideOfNumber, stringOfSide, turnEdge)

import Array exposing (Array)


type Color
    = White
    | Orange
    | Green
    | Red
    | Blue
    | Yellow


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


type alias Cube =
    { corner : Array ( Int, CornerOrientation )
    , edge : Array ( Int, EdgeOrientation )
    , center : Array Int
    }


type Side
    = Top
    | Left
    | Front
    | Right
    | Back
    | Bottom


sideOfNumber : Int -> Side
sideOfNumber n =
    case n of
        0 ->
            Top

        1 ->
            Left

        2 ->
            Front

        3 ->
            Right

        4 ->
            Back

        _ ->
            Bottom


stringOfSide : Side -> String
stringOfSide side =
    case side of
        Top ->
            "Top"

        Left ->
            "Left"

        Front ->
            "Front"

        Right ->
            "Right"

        Back ->
            "Back"

        Bottom ->
            "Bottom"


type Direction
    = CW
    | CCW


type Axis
    = X -- through Back to Front
    | Y -- through Left to Right
    | Z -- through Bottom to Top


rotate : { cornerPermutation : List ( Int, Int, CornerOrientation ), edgePermutation : List ( Int, Int, EdgeOrientation ), centerPremutation : List ( Int, Int ) } -> Cube -> Cube
rotate permutation cube =
    let
        nextCorner =
            permutation.cornerPermutation
                |> List.foldl
                    (\( idx, next, rot1 ) co ->
                        Array.set idx
                            (Array.get next cube.corner
                                |> Maybe.withDefault ( 0, NormalRotate )
                                |> (\( v, rot2 ) -> ( v, addCornerOrientation rot1 rot2 ))
                            )
                            co
                    )
                    cube.corner

        nextEdge =
            permutation.edgePermutation
                |> List.foldl
                    (\( idx, next, turn1 ) eo ->
                        Array.set idx
                            (Array.get next cube.edge
                                |> Maybe.withDefault ( 0, Normal )
                                |> (\( v, turn2 ) -> ( v, addEdgeOrientation turn1 turn2 ))
                            )
                            eo
                    )
                    cube.edge

        nextCenter =
            permutation.centerPremutation
                |> List.foldl
                    (\( idx, next ) co ->
                        Array.set idx
                            (Array.get next cube.center
                                |> Maybe.withDefault 0
                            )
                            co
                    )
                    cube.center
    in
    { cube | corner = nextCorner, edge = nextEdge, center = nextCenter }


rotateSide : Side -> Direction -> Cube -> Cube
rotateSide side direction cube =
    let
        ( cornerPermutation, edgePermutation ) =
            case ( side, direction ) of
                ( Top, CW ) ->
                    ( [ ( 0, 3, NormalRotate ), ( 1, 0, NormalRotate ), ( 2, 1, NormalRotate ), ( 3, 2, NormalRotate ) ]
                    , [ ( 0, 1, Normal ), ( 1, 2, Normal ), ( 2, 3, Normal ), ( 3, 0, Normal ) ]
                    )

                ( Top, CCW ) ->
                    ( [ ( 0, 1, NormalRotate ), ( 1, 2, NormalRotate ), ( 2, 3, NormalRotate ), ( 3, 0, NormalRotate ) ]
                    , [ ( 0, 3, Normal ), ( 1, 0, Normal ), ( 2, 1, Normal ), ( 3, 2, Normal ) ]
                    )

                ( Left, CW ) ->
                    ( [ ( 0, 4, LeftRotate ), ( 3, 0, RightRotate ), ( 7, 3, LeftRotate ), ( 4, 7, RightRotate ) ]
                    , [ ( 1, 11, Normal ), ( 8, 1, Normal ), ( 5, 8, Normal ), ( 11, 5, Normal ) ]
                    )

                ( Left, CCW ) ->
                    ( [ ( 0, 3, LeftRotate ), ( 3, 7, RightRotate ), ( 7, 4, LeftRotate ), ( 4, 0, RightRotate ) ]
                    , [ ( 1, 8, Normal ), ( 8, 5, Normal ), ( 5, 11, Normal ), ( 11, 1, Normal ) ]
                    )

                ( Front, CW ) ->
                    ( [ ( 3, 7, LeftRotate ), ( 2, 3, RightRotate ), ( 6, 2, LeftRotate ), ( 7, 6, RightRotate ) ]
                    , [ ( 2, 8, Reversed ), ( 9, 2, Reversed ), ( 6, 9, Reversed ), ( 8, 6, Reversed ) ]
                    )

                ( Front, CCW ) ->
                    ( [ ( 3, 2, LeftRotate ), ( 2, 6, RightRotate ), ( 6, 7, LeftRotate ), ( 7, 3, RightRotate ) ]
                    , [ ( 2, 9, Reversed ), ( 9, 6, Reversed ), ( 6, 8, Reversed ), ( 8, 2, Reversed ) ]
                    )

                ( Right, CW ) ->
                    ( [ ( 1, 2, RightRotate ), ( 2, 6, LeftRotate ), ( 6, 5, RightRotate ), ( 5, 1, LeftRotate ) ]
                    , [ ( 3, 9, Normal ), ( 10, 3, Normal ), ( 7, 10, Normal ), ( 9, 7, Normal ) ]
                    )

                ( Right, CCW ) ->
                    ( [ ( 1, 5, RightRotate ), ( 2, 1, LeftRotate ), ( 6, 2, RightRotate ), ( 5, 6, LeftRotate ) ]
                    , [ ( 3, 10, Normal ), ( 10, 7, Normal ), ( 7, 9, Normal ), ( 9, 3, Normal ) ]
                    )

                ( Back, CW ) ->
                    ( [ ( 0, 1, RightRotate ), ( 1, 5, LeftRotate ), ( 5, 4, RightRotate ), ( 4, 0, LeftRotate ) ]
                    , [ ( 11, 0, Reversed ), ( 4, 11, Reversed ), ( 10, 4, Reversed ), ( 0, 10, Reversed ) ]
                    )

                ( Back, CCW ) ->
                    ( [ ( 0, 4, RightRotate ), ( 1, 0, LeftRotate ), ( 5, 1, RightRotate ), ( 4, 5, LeftRotate ) ]
                    , [ ( 11, 4, Reversed ), ( 4, 10, Reversed ), ( 10, 0, Reversed ), ( 0, 11, Reversed ) ]
                    )

                ( Bottom, CW ) ->
                    ( [ ( 7, 4, NormalRotate ), ( 6, 7, NormalRotate ), ( 5, 6, NormalRotate ), ( 4, 5, NormalRotate ) ]
                    , [ ( 6, 5, Normal ), ( 7, 6, Normal ), ( 4, 7, Normal ), ( 5, 4, Normal ) ]
                    )

                ( Bottom, CCW ) ->
                    ( [ ( 7, 6, NormalRotate ), ( 6, 5, NormalRotate ), ( 5, 4, NormalRotate ), ( 4, 7, NormalRotate ) ]
                    , [ ( 6, 7, Normal ), ( 7, 4, Normal ), ( 4, 5, Normal ), ( 5, 6, Normal ) ]
                    )
    in
    rotate { cornerPermutation = cornerPermutation, edgePermutation = edgePermutation, centerPremutation = [] } cube


rotateMiddle : Axis -> Direction -> Cube -> Cube
rotateMiddle axis direction cube =
    let
        ( edgePermutation, centerPremutation ) =
            case ( axis, direction ) of
                ( X, CW ) ->
                    ( [ ( 1, 5, Reversed ), ( 5, 7, Reversed ), ( 7, 3, Reversed ), ( 3, 1, Reversed ) ]
                    , [ ( 0, 1 ), ( 1, 5 ), ( 5, 3 ), ( 3, 0 ) ]
                    )

                ( X, CCW ) ->
                    ( [ ( 5, 1, Reversed ), ( 7, 5, Reversed ), ( 3, 7, Reversed ), ( 1, 3, Reversed ) ]
                    , [ ( 1, 0 ), ( 5, 1 ), ( 3, 5 ), ( 0, 3 ) ]
                    )

                ( Y, CW ) ->
                    ( [ ( 2, 6, Reversed ), ( 6, 4, Reversed ), ( 4, 0, Reversed ), ( 0, 2, Reversed ) ]
                    , [ ( 0, 2 ), ( 2, 5 ), ( 5, 4 ), ( 4, 0 ) ]
                    )

                ( Y, CCW ) ->
                    ( [ ( 6, 2, Reversed ), ( 4, 6, Reversed ), ( 0, 4, Reversed ), ( 2, 0, Reversed ) ]
                    , [ ( 2, 0 ), ( 5, 2 ), ( 4, 5 ), ( 0, 4 ) ]
                    )

                ( Z, CW ) ->
                    ( [ ( 11, 8, Reversed ), ( 8, 9, Reversed ), ( 9, 10, Reversed ), ( 10, 11, Reversed ) ]
                    , [ ( 1, 2 ), ( 2, 3 ), ( 3, 4 ), ( 4, 1 ) ]
                    )

                ( Z, CCW ) ->
                    ( [ ( 8, 11, Reversed ), ( 9, 8, Reversed ), ( 10, 9, Reversed ), ( 11, 10, Reversed ) ]
                    , [ ( 2, 1 ), ( 3, 2 ), ( 4, 3 ), ( 1, 4 ) ]
                    )
    in
    rotate { cornerPermutation = [], edgePermutation = edgePermutation, centerPremutation = centerPremutation } cube


init : () -> Cube
init _ =
    Cube
        (Array.fromList <| List.map (\i -> ( i, NormalRotate )) <| List.range 0 7)
        (Array.fromList <| List.map (\i -> ( i, Normal )) <| List.range 0 11)
        (Array.fromList <| List.range 0 5)
