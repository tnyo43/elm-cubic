module Cube exposing (Color(..), CornerOrientation(..), Cube, EdgeOrientation(..), Side(..), init, rotate, rotateCorner, sideOfNumber, stringOfSide, turnEdge)

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
    }


type Side
    = Top
    | Left
    | Front
    | Right
    | Back
    | Down


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
            Down


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

        Down ->
            "Dowb"


rotate : Side -> Cube -> Cube
rotate side cube =
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
                            (Array.get next cube.corner
                                |> Maybe.withDefault ( 0, NormalRotate )
                                |> (\( v, rot2 ) -> ( v, addCornerOrientation rot1 rot2 ))
                            )
                            co
                    )
                    cube.corner

        nextEdge =
            edgePermutation
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
    in
    { cube | corner = nextCorner, edge = nextEdge }


init : () -> Cube
init _ =
    Cube
        (Array.fromList <| List.map (\i -> ( i, NormalRotate )) <| List.range 0 7)
        (Array.fromList <| List.map (\i -> ( i, Normal )) <| List.range 0 11)
