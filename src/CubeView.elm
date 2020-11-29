module CubeView exposing (..)

import Angle
import Array exposing (..)
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


sidePanel : Array Color -> Int -> Entity coordinate
sidePanel colors side =
    let
        createPanel color =
            Scene3d.quad (Material.color <| ofColor color)
                (Point3d.meters -0.45 -0.45 0)
                (Point3d.meters 0.45 -0.45 0)
                (Point3d.meters 0.45 0.45 0)
                (Point3d.meters -0.45 0.45 0)

        pos i =
            Vector3d.meters (i // 3 - 1 |> toFloat) (modBy 3 i - 1 |> toFloat) 1.51

        rotate =
            case side of
                0 ->
                    identity

                1 ->
                    rotateAround Axis3d.y (Angle.degrees 90) >> rotateAround Axis3d.z (Angle.degrees -90)

                2 ->
                    rotateAround Axis3d.y (Angle.degrees 90)

                3 ->
                    rotateAround Axis3d.y (Angle.degrees 90) >> rotateAround Axis3d.z (Angle.degrees 90)

                4 ->
                    rotateAround Axis3d.y (Angle.degrees -90) >> rotateAround Axis3d.x (Angle.degrees 180)

                _ ->
                    rotateAround Axis3d.x (Angle.degrees -180) >> rotateAround Axis3d.z (Angle.degrees 180)
    in
    Array.indexedMap (\i color -> translateBy (pos i) (createPanel color)) colors
        |> Array.toList
        |> group
        |> rotate


cubeOfEntiry : Cube -> Entity coordinate
cubeOfEntiry cube =
    let
        createBlock =
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
                |> List.map (\( x, y, z ) -> createBlock (toFloat x) (toFloat y) (toFloat z))

        sides =
            List.range 0 5
                |> List.map (\i -> sidePanel (sideOf cube i) i)
    in
    List.append blocks sides
        --List.append [] sides
        |> group


ofEntity : Data -> Entity coordinate
ofEntity data =
    ofData data |> cubeOfEntiry
