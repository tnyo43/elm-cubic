module ConvexHull exposing (isInConvexArea)

import Array exposing (Array)
import Convex exposing (convexHull)
import Math.Vector2 as Vector2 exposing (Vec2)
import Utils exposing (intersection)


countIntersectEdgeWithHorizontalHalfLine : Array Vec2 -> Vec2 -> Int
countIntersectEdgeWithHorizontalHalfLine points target =
    let
        isEdgeIntersectWithHorizontalHalfLine point1_ point2_ =
            let
                ( point1, point2 ) =
                    if Vector2.getY point1_ > Vector2.getY point2_ then
                        ( point2_, point1_ )

                    else
                        ( point1_, point2_ )
            in
            (Vector2.getY point2 > Vector2.getY target)
                && (Vector2.getY point1 < Vector2.getY target)
                && (case intersection { p1 = point1, p2 = point2 } { p1 = target, p2 = Vector2.add target (Vector2.vec2 1 0) } of
                        Nothing ->
                            False

                        Just p ->
                            Vector2.getX p > Vector2.getX target
                   )
    in
    Array.indexedMap Tuple.pair points
        |> Array.foldl
            (\( i, p1 ) acc ->
                acc
                    + (case Array.get (i + 1) points of
                        Nothing ->
                            0

                        Just p2 ->
                            if isEdgeIntersectWithHorizontalHalfLine p1 p2 then
                                1

                            else
                                0
                      )
            )
            0


isInConvexArea : List { x : Float, y : Float } -> { x : Float, y : Float } -> Bool
isInConvexArea points target =
    List.map (\{ x, y } -> Vector2.vec2 x y) points
        |> convexHull
        |> Array.fromList
        |> (\arrayPoints -> countIntersectEdgeWithHorizontalHalfLine arrayPoints (Vector2.vec2 target.x target.y))
        |> modBy 2
        |> (==) 1
