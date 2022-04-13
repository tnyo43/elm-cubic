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


panel_size : Float
panel_size =
    0.9


small_gap : Float
small_gap =
    0.01


colorsOfPosition : Cube -> ( Int, Int, Int ) -> Array (Maybe Color)
colorsOfPosition cube ( x, y, z ) =
    -- Top Left Front Right Back Down の順になる
    [ -- TOP
      if z == 1 then
        Array.get (y + (x + 1) * 3 + 1 + 9 * 0) cube

      else
        Nothing
    , -- Left
      if y == -1 then
        Array.get (x + (z - 1) * -3 + 1 + 9 * 1) cube

      else
        Nothing
    , -- Front
      if x == 1 then
        Array.get (y + (z - 1) * -3 + 1 + 9 * 2) cube

      else
        Nothing
    , -- Right
      if y == 1 then
        Array.get (-x + (z - 1) * -3 + 1 + 9 * 3) cube

      else
        Nothing
    , -- Back
      if x == -1 then
        Array.get (-y + (z - 1) * -3 + 1 + 9 * 4) cube

      else
        Nothing
    , --Down
      if z == -1 then
        Array.get (y + (x - 1) * -3 + 1 + 9 * 5) cube

      else
        Nothing
    ]
        |> Array.fromList



-- blockOfPosition : Cube -> ( Int, Int, Int ) -> Entity coordinate
-- blockOfPosition cube position =
--     let
--         panels = colorsOfPosition position
--         top =


sidePanel : Array Color -> Side -> Entity coordinate
sidePanel colors side =
    let
        createPanel color =
            Scene3d.quad (Material.color <| ofColor color)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)
                |> Scene3d.scaleAbout Point3d.origin (panel_size / 2)

        pos i =
            Vector3d.meters (i // 3 - 1 |> toFloat) (modBy 3 i - 1 |> toFloat) (1 / 2 * 3 + small_gap)

        rotate =
            case side of
                Top ->
                    identity

                Left ->
                    rotateAround Axis3d.y (Angle.degrees 90) >> rotateAround Axis3d.z (Angle.degrees -90)

                Front ->
                    rotateAround Axis3d.y (Angle.degrees 90)

                Right ->
                    rotateAround Axis3d.y (Angle.degrees 90) >> rotateAround Axis3d.z (Angle.degrees 90)

                Back ->
                    rotateAround Axis3d.y (Angle.degrees -90) >> rotateAround Axis3d.x (Angle.degrees 180)

                Down ->
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
                        { x1 = Length.meters 1
                        , x2 = Length.meters -1
                        , y1 = Length.meters 1
                        , y2 = Length.meters -1
                        , z1 = Length.meters 1
                        , z2 = Length.meters -1
                        }
                        |> Block3d.scaleAbout Point3d.origin (1 / 2)
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
            [ Top
            , Left
            , Front
            , Right
            , Back
            , Down
            ]
                |> List.map (\side -> sidePanel (sideOf cube side) side)
    in
    List.append blocks sides
        --List.append [] sides
        |> group


ofEntity : Data -> Entity coordinate
ofEntity data =
    ofData data
        |> cubeOfEntiry
