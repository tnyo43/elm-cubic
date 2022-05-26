module Utils exposing (..)


cross : (a -> b -> c) -> List a -> List b -> List c
cross f xs ys =
    List.concatMap
        (\x -> List.map (\y -> f x y) ys)
        xs


toIntPoint2d : { x : Float, y : Float } -> { x : Int, y : Int }
toIntPoint2d { x, y } =
    { x = round x, y = round y }


distance : { x : Float, y : Float } -> { x : Float, y : Float } -> Float
distance a b =
    (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y) |> sqrt
