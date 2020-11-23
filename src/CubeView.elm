module CubeView exposing (..)

import Angle
import Axis3d
import Block3d
import Color as ObjColor
import Cube exposing (..)
import Length
import Point3d
import Scene3d exposing (..)
import Scene3d.Material as Material
import Utils exposing (..)
import Vector3d


makeSide : Color -> Entity coordinate
makeSide color =
    Scene3d.quad (Material.color <| ofColor color)
        (Point3d.meters -0.45 -0.45 0)
        (Point3d.meters 0.45 -0.45 0)
        (Point3d.meters 0.45 0.45 0)
        (Point3d.meters -0.45 0.45 0)


defaultSide : Color -> List (Entity coordinate)
defaultSide color =
    let
        panels =
            cross
                (\x y -> Vector3d.meters (toFloat x) (toFloat y) 0)
                (List.range -1 1)
                (List.range -1 1)
                |> List.map (\v -> translateBy v <| makeSide color)
    in
    case color of
        White ->
            List.map (rotateAround Axis3d.y (Angle.degrees 90) >> translateBy (Vector3d.meters -1.51 0 0)) panels

        Orange ->
            List.map (rotateAround Axis3d.x (Angle.degrees 90) >> translateBy (Vector3d.meters 0 -1.51 0)) panels

        Green ->
            List.map (translateBy (Vector3d.meters 0 0 1.51)) panels

        Red ->
            List.map (rotateAround Axis3d.x (Angle.degrees 90) >> translateBy (Vector3d.meters 0 1.51 0)) panels

        Blue ->
            List.map (translateBy (Vector3d.meters 0 0 -1.51)) panels

        Yellow ->
            List.map (rotateAround Axis3d.y (Angle.degrees 90) >> translateBy (Vector3d.meters 1.51 0 0)) panels


defaultBlocks : Cube -> Entity coordinate
defaultBlocks cube =
    let
        makeBlock =
            \x y z ->
                Scene3d.block
                    (Material.color ObjColor.black)
                    (Block3d.with
                        { x1 = Length.meters 0.5
                        , x2 = Length.meters -0.5
                        , y1 = Length.meters 0.5
                        , y2 = Length.meters -0.5
                        , z1 = Length.meters 0.5
                        , z2 = Length.meters -0.5
                        }
                    )
                    |> translateBy
                        (Vector3d.meters x y z)

        blocks =
            cross (\x y -> ( x, y ))
                (List.range -1 1)
                (List.range -1 1)
                |> cross (\z ( x, y ) -> ( x, y, z ))
                    (List.range -1 1)
                |> List.filter ((/=) ( 0, 0, 0 ))
                |> List.map (\( x, y, z ) -> makeBlock (toFloat x) (toFloat y) (toFloat z))

        sides =
            List.concatMap defaultSide
                [ White, Green, Orange, Red, Blue, Yellow ]
    in
    List.append blocks sides
        |> group


ofEntity : Cube -> Entity coordinate
ofEntity cube =
    defaultBlocks cube
