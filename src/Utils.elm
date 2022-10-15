module Utils exposing (..)

import Math.Vector2 as Vector2 exposing (Vec2)


cross : (a -> b -> c) -> List a -> List b -> List c
cross f xs ys =
    List.concatMap
        (\x -> List.map (\y -> f x y) ys)
        xs


toIntPoint2d : { x : Float, y : Float } -> { x : Int, y : Int }
toIntPoint2d { x, y } =
    { x = round x, y = round y }


distance : { x : Float, y : Float } -> { x : Float, y : Float } -> Float
distance p1 p2 =
    (p1.x - p2.x) ^ 2 + (p1.y - p2.y) ^ 2 |> sqrt


type alias Line =
    { p1 : Vec2, p2 : Vec2 }


intersection : Line -> Line -> Maybe Vec2
intersection l1 l2 =
    let
        vAC =
            Vector2.sub l2.p1 l1.p1

        vAB =
            Vector2.sub l1.p2 l1.p1

        vCD =
            Vector2.sub l2.p2 l2.p1

        a =
            Vector2.getX vAB * Vector2.getY vCD - Vector2.getY vAB * Vector2.getX vCD
    in
    if a == 0 then
        Nothing

    else
        let
            s =
                (Vector2.getY vCD * Vector2.getX vAC - Vector2.getX vCD * Vector2.getY vAC) / a
        in
        Vector2.add l1.p1 (Vector2.scale s vAB) |> Just
