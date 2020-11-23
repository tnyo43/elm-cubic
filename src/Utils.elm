module Utils exposing (..)


cross : (a -> b -> c) -> List a -> List b -> List c
cross f xs ys =
    List.concatMap
        (\x -> List.map (\y -> f x y) ys)
        xs
