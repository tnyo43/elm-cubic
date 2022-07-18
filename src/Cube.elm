module Cube exposing (Axis(..), Color(..), CornerOrientation(..), Cube, Direction(..), EdgeOrientation(..), Side(..), init, rotateCorner, rotateMiddle, rotateSide, sideOfNumber, stringOfSide, turnEdge)

import Array exposing (Array)


type Color
    = White
    | Orange
    | Green
    | Red
    | Blue
    | Yellow


type Direction
    = CW
    | CCW


type Side
    = Top
    | Left
    | Front
    | Right
    | Back
    | Bottom


type Axis
    = X -- through Back to Front
    | Y -- through Left to Right
    | Z -- through Bottom to Top


type CornerOrientation
    = NormalCO
    | RightCO
    | LeftCO


type EdgeOrientation
    = NormalEO
    | ReversedEO


type alias Cube =
    { corner : Array ( Int, CornerOrientation )
    , edge : Array ( Int, EdgeOrientation )
    , center : Array Int
    }


addCornerOrientation : CornerOrientation -> CornerOrientation -> CornerOrientation
addCornerOrientation o1 o2 =
    case ( o1, o2 ) of
        ( NormalCO, _ ) ->
            o2

        ( _, NormalCO ) ->
            o1

        ( LeftCO, LeftCO ) ->
            RightCO

        ( RightCO, RightCO ) ->
            LeftCO

        _ ->
            NormalCO


rotateCorner : CornerOrientation -> ( Color, Color, Color ) -> ( Color, Color, Color )
rotateCorner co ( c1, c2, c3 ) =
    case co of
        NormalCO ->
            ( c1, c2, c3 )

        RightCO ->
            ( c3, c1, c2 )

        LeftCO ->
            ( c2, c3, c1 )


addEdgeOrientation : EdgeOrientation -> EdgeOrientation -> EdgeOrientation
addEdgeOrientation e1 e2 =
    if e1 == e2 then
        NormalEO

    else
        ReversedEO


turnEdge : EdgeOrientation -> ( Color, Color ) -> ( Color, Color )
turnEdge eo ( c1, c2 ) =
    case eo of
        NormalEO ->
            ( c1, c2 )

        ReversedEO ->
            ( c2, c1 )


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


rotate : { cornerPermutation : List ( Int, Int, CornerOrientation ), edgePermutation : List ( Int, Int, EdgeOrientation ), centerPremutation : List ( Int, Int ) } -> Cube -> Cube
rotate permutation cube =
    let
        nextCorner =
            permutation.cornerPermutation
                |> List.foldl
                    (\( idx, next, rot1 ) co ->
                        Array.set idx
                            (Array.get next cube.corner
                                |> Maybe.withDefault ( 0, NormalCO )
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
                                |> Maybe.withDefault ( 0, NormalEO )
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
                    ( [ ( 0, 3, NormalCO ), ( 1, 0, NormalCO ), ( 2, 1, NormalCO ), ( 3, 2, NormalCO ) ]
                    , [ ( 0, 1, NormalEO ), ( 1, 2, NormalEO ), ( 2, 3, NormalEO ), ( 3, 0, NormalEO ) ]
                    )

                ( Top, CCW ) ->
                    ( [ ( 0, 1, NormalCO ), ( 1, 2, NormalCO ), ( 2, 3, NormalCO ), ( 3, 0, NormalCO ) ]
                    , [ ( 0, 3, NormalEO ), ( 1, 0, NormalEO ), ( 2, 1, NormalEO ), ( 3, 2, NormalEO ) ]
                    )

                ( Left, CW ) ->
                    ( [ ( 0, 4, LeftCO ), ( 3, 0, RightCO ), ( 7, 3, LeftCO ), ( 4, 7, RightCO ) ]
                    , [ ( 1, 11, NormalEO ), ( 8, 1, NormalEO ), ( 5, 8, NormalEO ), ( 11, 5, NormalEO ) ]
                    )

                ( Left, CCW ) ->
                    ( [ ( 0, 3, LeftCO ), ( 3, 7, RightCO ), ( 7, 4, LeftCO ), ( 4, 0, RightCO ) ]
                    , [ ( 1, 8, NormalEO ), ( 8, 5, NormalEO ), ( 5, 11, NormalEO ), ( 11, 1, NormalEO ) ]
                    )

                ( Front, CW ) ->
                    ( [ ( 3, 7, LeftCO ), ( 2, 3, RightCO ), ( 6, 2, LeftCO ), ( 7, 6, RightCO ) ]
                    , [ ( 2, 8, ReversedEO ), ( 9, 2, ReversedEO ), ( 6, 9, ReversedEO ), ( 8, 6, ReversedEO ) ]
                    )

                ( Front, CCW ) ->
                    ( [ ( 3, 2, LeftCO ), ( 2, 6, RightCO ), ( 6, 7, LeftCO ), ( 7, 3, RightCO ) ]
                    , [ ( 2, 9, ReversedEO ), ( 9, 6, ReversedEO ), ( 6, 8, ReversedEO ), ( 8, 2, ReversedEO ) ]
                    )

                ( Right, CW ) ->
                    ( [ ( 1, 2, RightCO ), ( 2, 6, LeftCO ), ( 6, 5, RightCO ), ( 5, 1, LeftCO ) ]
                    , [ ( 3, 9, NormalEO ), ( 10, 3, NormalEO ), ( 7, 10, NormalEO ), ( 9, 7, NormalEO ) ]
                    )

                ( Right, CCW ) ->
                    ( [ ( 1, 5, RightCO ), ( 2, 1, LeftCO ), ( 6, 2, RightCO ), ( 5, 6, LeftCO ) ]
                    , [ ( 3, 10, NormalEO ), ( 10, 7, NormalEO ), ( 7, 9, NormalEO ), ( 9, 3, NormalEO ) ]
                    )

                ( Back, CW ) ->
                    ( [ ( 0, 1, RightCO ), ( 1, 5, LeftCO ), ( 5, 4, RightCO ), ( 4, 0, LeftCO ) ]
                    , [ ( 11, 0, ReversedEO ), ( 4, 11, ReversedEO ), ( 10, 4, ReversedEO ), ( 0, 10, ReversedEO ) ]
                    )

                ( Back, CCW ) ->
                    ( [ ( 0, 4, RightCO ), ( 1, 0, LeftCO ), ( 5, 1, RightCO ), ( 4, 5, LeftCO ) ]
                    , [ ( 11, 4, ReversedEO ), ( 4, 10, ReversedEO ), ( 10, 0, ReversedEO ), ( 0, 11, ReversedEO ) ]
                    )

                ( Bottom, CW ) ->
                    ( [ ( 7, 4, NormalCO ), ( 6, 7, NormalCO ), ( 5, 6, NormalCO ), ( 4, 5, NormalCO ) ]
                    , [ ( 6, 5, NormalEO ), ( 7, 6, NormalEO ), ( 4, 7, NormalEO ), ( 5, 4, NormalEO ) ]
                    )

                ( Bottom, CCW ) ->
                    ( [ ( 7, 6, NormalCO ), ( 6, 5, NormalCO ), ( 5, 4, NormalCO ), ( 4, 7, NormalCO ) ]
                    , [ ( 6, 7, NormalEO ), ( 7, 4, NormalEO ), ( 4, 5, NormalEO ), ( 5, 6, NormalEO ) ]
                    )
    in
    rotate { cornerPermutation = cornerPermutation, edgePermutation = edgePermutation, centerPremutation = [] } cube


rotateMiddle : Axis -> Direction -> Cube -> Cube
rotateMiddle axis direction cube =
    let
        ( edgePermutation, centerPremutation ) =
            case ( axis, direction ) of
                ( X, CW ) ->
                    ( [ ( 1, 5, ReversedEO ), ( 5, 7, ReversedEO ), ( 7, 3, ReversedEO ), ( 3, 1, ReversedEO ) ]
                    , [ ( 0, 1 ), ( 1, 5 ), ( 5, 3 ), ( 3, 0 ) ]
                    )

                ( X, CCW ) ->
                    ( [ ( 5, 1, ReversedEO ), ( 7, 5, ReversedEO ), ( 3, 7, ReversedEO ), ( 1, 3, ReversedEO ) ]
                    , [ ( 1, 0 ), ( 5, 1 ), ( 3, 5 ), ( 0, 3 ) ]
                    )

                ( Y, CW ) ->
                    ( [ ( 2, 6, ReversedEO ), ( 6, 4, ReversedEO ), ( 4, 0, ReversedEO ), ( 0, 2, ReversedEO ) ]
                    , [ ( 0, 2 ), ( 2, 5 ), ( 5, 4 ), ( 4, 0 ) ]
                    )

                ( Y, CCW ) ->
                    ( [ ( 6, 2, ReversedEO ), ( 4, 6, ReversedEO ), ( 0, 4, ReversedEO ), ( 2, 0, ReversedEO ) ]
                    , [ ( 2, 0 ), ( 5, 2 ), ( 4, 5 ), ( 0, 4 ) ]
                    )

                ( Z, CW ) ->
                    ( [ ( 11, 8, ReversedEO ), ( 8, 9, ReversedEO ), ( 9, 10, ReversedEO ), ( 10, 11, ReversedEO ) ]
                    , [ ( 1, 2 ), ( 2, 3 ), ( 3, 4 ), ( 4, 1 ) ]
                    )

                ( Z, CCW ) ->
                    ( [ ( 8, 11, ReversedEO ), ( 9, 8, ReversedEO ), ( 10, 9, ReversedEO ), ( 11, 10, ReversedEO ) ]
                    , [ ( 2, 1 ), ( 3, 2 ), ( 4, 3 ), ( 1, 4 ) ]
                    )
    in
    rotate { cornerPermutation = [], edgePermutation = edgePermutation, centerPremutation = centerPremutation } cube


init : () -> Cube
init _ =
    Cube
        (Array.fromList <| List.map (\i -> ( i, NormalCO )) <| List.range 0 7)
        (Array.fromList <| List.map (\i -> ( i, NormalEO )) <| List.range 0 11)
        (Array.fromList <| List.range 0 5)
