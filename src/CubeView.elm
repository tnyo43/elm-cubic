module CubeView exposing (CubeColors, GlobalRotation, colorsOfPosition, cubeView, initGlobalRotation, mouseOveredObject, ofCube, rotateAnimationTime, stringOfSelectedObject, updateGlobalRotation)

import Angle
import Array exposing (..)
import Axis3d
import Block3d
import Color as ObjColor
import ConvexHull exposing (isInConvexArea)
import Cube exposing (Color(..), CornerOrientation(..), Cube, Direction(..), EdgeOrientation(..), Side(..), rotateCorner, sideOfNumber, stringOfSide, turnEdge)
import Length
import Point3d
import Quaternion exposing (Quaternion)
import Scene3d exposing (..)
import Scene3d.Material as Material
import Utils exposing (..)
import Vector exposing (Vector)
import Vector3d



-- View Parameters


panel_size : Float
panel_size =
    0.9


small_gap : Float
small_gap =
    0.01


ofColor : Color -> ObjColor.Color
ofColor color =
    case color of
        White ->
            ObjColor.white

        Orange ->
            ObjColor.orange

        Green ->
            ObjColor.green

        Red ->
            ObjColor.red

        Blue ->
            ObjColor.blue

        Yellow ->
            ObjColor.yellow


rotateAnimationTime : Int
rotateAnimationTime =
    400



-- Cube -> Entity


type alias CubeColors =
    Array Color


colorsOfPosition : CubeColors -> ( Int, Int, Int ) -> List (Maybe Color)
colorsOfPosition cubeColors ( x, y, z ) =
    -- Top Left Front Right Back Down の順になる
    [ -- TOP
      if z == 1 then
        Array.get (y + (x + 1) * 3 + 1 + 9 * 0) cubeColors

      else
        Nothing
    , -- Left
      if y == -1 then
        Array.get (x + (z - 1) * -3 + 1 + 9 * 1) cubeColors

      else
        Nothing
    , -- Front
      if x == 1 then
        Array.get (y + (z - 1) * -3 + 1 + 9 * 2) cubeColors

      else
        Nothing
    , -- Right
      if y == 1 then
        Array.get (-x + (z - 1) * -3 + 1 + 9 * 3) cubeColors

      else
        Nothing
    , -- Back
      if x == -1 then
        Array.get (-y + (z - 1) * -3 + 1 + 9 * 4) cubeColors

      else
        Nothing
    , --Down
      if z == -1 then
        Array.get (y + (x - 1) * -3 + 1 + 9 * 5) cubeColors

      else
        Nothing
    ]


blockOfPosition : CubeColors -> ( Int, Int, Int ) -> Entity coordinate
blockOfPosition cubeColors position =
    let
        ( x, y, z ) =
            position

        rotate side =
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
                    rotateAround Axis3d.x (Angle.degrees 180) >> rotateAround Axis3d.z (Angle.degrees 180)
    in
    colorsOfPosition cubeColors position
        |> List.indexedMap
            (\i c ->
                case c of
                    Just color ->
                        Scene3d.quad (Material.color <| ofColor color)
                            (Point3d.meters -1 -1 0)
                            (Point3d.meters 1 -1 0)
                            (Point3d.meters 1 1 0)
                            (Point3d.meters -1 1 0)
                            |> Scene3d.scaleAbout Point3d.origin (panel_size / 2)
                            |> Scene3d.translateBy (Vector3d.meters 0 0 (0.5 + small_gap))
                            |> rotate (sideOfNumber i)

                    Nothing ->
                        Scene3d.nothing
            )
        |> (::)
            (Scene3d.block
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
            )
        |> Scene3d.group
        |> translateBy
            (Vector3d.meters (toFloat x) (toFloat y) (toFloat z))


entityOfCubeColors : Maybe ( Side, Direction, Float ) -> CubeColors -> Entity coordinate
entityOfCubeColors rotatingSide cubeColors =
    let
        isRotating ( x, y, z ) =
            case rotatingSide of
                Nothing ->
                    False

                Just ( side, _, _ ) ->
                    case side of
                        Top ->
                            z == 1

                        Left ->
                            y == -1

                        Front ->
                            x == 1

                        Right ->
                            y == 1

                        Back ->
                            x == -1

                        Down ->
                            z == -1

        rotate =
            case rotatingSide of
                Nothing ->
                    identity

                Just ( side, direction, ratio ) ->
                    case ( side, direction ) of
                        ( Top, CW ) ->
                            (-90 * ratio) |> Angle.degrees |> rotateAround Axis3d.z

                        ( Top, CCW ) ->
                            (90 * ratio) |> Angle.degrees |> rotateAround Axis3d.z

                        ( Left, CW ) ->
                            (90 * ratio) |> Angle.degrees |> rotateAround Axis3d.y

                        ( Left, CCW ) ->
                            (-90 * ratio) |> Angle.degrees |> rotateAround Axis3d.y

                        ( Front, CW ) ->
                            (-90 * ratio) |> Angle.degrees |> rotateAround Axis3d.x

                        ( Front, CCW ) ->
                            (90 * ratio) |> Angle.degrees |> rotateAround Axis3d.x

                        ( Right, CW ) ->
                            (-90 * ratio) |> Angle.degrees |> rotateAround Axis3d.y

                        ( Right, CCW ) ->
                            (90 * ratio) |> Angle.degrees |> rotateAround Axis3d.y

                        ( Back, CW ) ->
                            (90 * ratio) |> Angle.degrees |> rotateAround Axis3d.x

                        ( Back, CCW ) ->
                            (-90 * ratio) |> Angle.degrees |> rotateAround Axis3d.x

                        ( Down, CW ) ->
                            (90 * ratio) |> Angle.degrees |> rotateAround Axis3d.z

                        ( Down, CCW ) ->
                            (-90 * ratio) |> Angle.degrees |> rotateAround Axis3d.z
    in
    cross (\x y -> ( x, y ))
        (List.range -1 1)
        (List.range -1 1)
        |> cross (\z ( x, y ) -> ( x, y, z ))
            (List.range -1 1)
        |> List.filter ((/=) ( 0, 0, 0 ))
        |> List.foldl
            (\pos ( blocks, rotatingBlocks ) ->
                let
                    block =
                        blockOfPosition cubeColors pos
                in
                if isRotating pos then
                    ( blocks, block :: rotatingBlocks )

                else
                    ( block :: blocks, rotatingBlocks )
            )
            ( [], [] )
        |> Tuple.mapBoth group (group >> rotate)
        |> (\( blocks, rotatingBlocks ) -> group [ blocks, rotatingBlocks ])


type alias Position =
    ( Int, Int )


indexOfPosition : Position -> Int
indexOfPosition ( p1, p2 ) =
    p1 * 9 + p2


cornerPosition : Int -> ( Position, Position, Position )
cornerPosition n =
    case n of
        0 ->
            ( ( 0, 0 ), ( 1, 0 ), ( 4, 2 ) )

        1 ->
            ( ( 0, 2 ), ( 4, 0 ), ( 3, 2 ) )

        2 ->
            ( ( 0, 8 ), ( 3, 0 ), ( 2, 2 ) )

        3 ->
            ( ( 0, 6 ), ( 2, 0 ), ( 1, 2 ) )

        4 ->
            ( ( 5, 6 ), ( 4, 8 ), ( 1, 6 ) )

        5 ->
            ( ( 5, 8 ), ( 3, 8 ), ( 4, 6 ) )

        6 ->
            ( ( 5, 2 ), ( 2, 8 ), ( 3, 6 ) )

        7 ->
            ( ( 5, 0 ), ( 1, 8 ), ( 2, 6 ) )

        _ ->
            ( ( -1, -1 ), ( -1, -1 ), ( -1, -1 ) )


edgePosition : Int -> ( Position, Position )
edgePosition n =
    case n of
        0 ->
            ( ( 0, 1 ), ( 4, 1 ) )

        1 ->
            ( ( 0, 3 ), ( 1, 1 ) )

        2 ->
            ( ( 0, 7 ), ( 2, 1 ) )

        3 ->
            ( ( 0, 5 ), ( 3, 1 ) )

        4 ->
            ( ( 5, 7 ), ( 4, 7 ) )

        5 ->
            ( ( 5, 3 ), ( 1, 7 ) )

        6 ->
            ( ( 5, 1 ), ( 2, 7 ) )

        7 ->
            ( ( 5, 5 ), ( 3, 7 ) )

        8 ->
            ( ( 2, 3 ), ( 1, 5 ) )

        9 ->
            ( ( 2, 5 ), ( 3, 3 ) )

        10 ->
            ( ( 4, 3 ), ( 3, 5 ) )

        11 ->
            ( ( 4, 5 ), ( 1, 3 ) )

        _ ->
            ( ( -1, -1 ), ( -1, -1 ) )


corner : Cube -> Int -> ( Color, Color, Color )
corner cube n =
    let
        ( block, rot ) =
            Array.get n cube.corner |> Maybe.withDefault ( -1, NormalRotate )

        colors =
            case block of
                0 ->
                    ( White, Orange, Blue )

                1 ->
                    ( White, Blue, Red )

                2 ->
                    ( White, Red, Green )

                3 ->
                    ( White, Green, Orange )

                4 ->
                    ( Yellow, Blue, Orange )

                5 ->
                    ( Yellow, Red, Blue )

                6 ->
                    ( Yellow, Green, Red )

                7 ->
                    ( Yellow, Orange, Green )

                _ ->
                    ( White, White, White )
    in
    rotateCorner rot colors


edge : Cube.Cube -> Int -> ( Color, Color )
edge cube n =
    let
        ( block, rot ) =
            Array.get n cube.edge |> Maybe.withDefault ( -1, Normal )

        colors =
            case block of
                0 ->
                    ( White, Blue )

                1 ->
                    ( White, Orange )

                2 ->
                    ( White, Green )

                3 ->
                    ( White, Red )

                4 ->
                    ( Yellow, Blue )

                5 ->
                    ( Yellow, Orange )

                6 ->
                    ( Yellow, Green )

                7 ->
                    ( Yellow, Red )

                8 ->
                    ( Green, Orange )

                9 ->
                    ( Green, Red )

                10 ->
                    ( Blue, Red )

                11 ->
                    ( Blue, Orange )

                _ ->
                    ( White, White )
    in
    turnEdge rot colors


setCornerColor : Cube -> Int -> CubeColors -> CubeColors
setCornerColor cube i cubeColors =
    let
        ( c1, c2, c3 ) =
            corner cube i

        ( p1, p2, p3 ) =
            cornerPosition i
    in
    [ ( c1, p1 ), ( c2, p2 ), ( c3, p3 ) ]
        |> List.foldl (\( c, p ) cub -> Array.set (indexOfPosition p) c cub) cubeColors


setEdgeColor : Cube -> Int -> CubeColors -> CubeColors
setEdgeColor cube i cubeColors =
    let
        ( c1, c2 ) =
            edge cube i

        ( p1, p2 ) =
            edgePosition i
    in
    [ ( c1, p1 ), ( c2, p2 ) ]
        |> List.foldl (\( c, p ) cub -> Array.set (indexOfPosition p) c cub) cubeColors


ofCube : Cube -> CubeColors
ofCube cube =
    let
        setCornerColors cubeColors =
            List.foldl
                (\i cub -> setCornerColor cube i cub)
                cubeColors
                (List.range 0 7)

        setEdgeColors cubeColors =
            List.foldl
                (\i cub -> setEdgeColor cube i cub)
                cubeColors
                (List.range 0 11)
    in
    List.indexedMap (\i c -> ( i, c )) [ White, Orange, Green, Red, Blue, Yellow ]
        |> List.foldl
            (\( i, color ) cub -> Array.set (indexOfPosition ( i, 4 )) color cub)
            (Array.repeat (9 * 6) White)
        |> setCornerColors
        |> setEdgeColors



-- Global Rotation


type alias GlobalRotation =
    Quaternion


initGlobalRotation : () -> GlobalRotation
initGlobalRotation () =
    Quaternion.identity


updateGlobalRotation : { dx : Int, dy : Int } -> GlobalRotation -> GlobalRotation
updateGlobalRotation { dx, dy } q =
    q |> Quaternion.mul (Quaternion.zRotation (toFloat dx * 0.005)) |> Quaternion.mul (Quaternion.yRotation (toFloat dy * 0.005))


toEulerAngles : GlobalRotation -> { roll : Float, pitch : Float, yaw : Float }
toEulerAngles q =
    let
        w =
            Quaternion.getW q

        x =
            Quaternion.getX q

        y =
            Quaternion.getY q

        z =
            Quaternion.getZ q

        roll =
            Basics.atan2 (2 * (w * x + y * z)) (w * w - x * x - y * y + z * z)

        pitch =
            Basics.asin (2 * (w * y - x * z))

        yaw =
            Basics.atan2 (2 * (w * z + x * y)) (w * w + x * x - y * y - z * z)
    in
    { roll = roll, pitch = pitch, yaw = yaw }


globalRotateWithEulerAngles : { roll : Float, pitch : Float, yaw : Float } -> Entity coordinate -> Entity coordinate
globalRotateWithEulerAngles { roll, pitch, yaw } entity =
    entity
        |> Scene3d.rotateAround Axis3d.x (Angle.radians roll)
        |> Scene3d.rotateAround Axis3d.y (Angle.radians pitch)
        |> Scene3d.rotateAround Axis3d.z (Angle.radians yaw)


type alias Positions =
    { corner : List Vector
    , edge : List Vector
    , center : List Vector
    }


initialPositions : Positions
initialPositions =
    { corner =
        [ -- Back & Left & Top
          Vector.vector -1 -1 1
        , -- Back & Right & Top
          Vector.vector -1 1 1
        , -- Front & Right & Top
          Vector.vector 1 1 1
        , -- Front & Left & Top
          Vector.vector 1 -1 1
        , -- Back & Left & Down
          Vector.vector -1 -1 -1
        , -- Back & Right & Down
          Vector.vector -1 1 -1
        , -- Front & Right & Down
          Vector.vector 1 1 -1
        , -- Front & Left & Down
          Vector.vector 1 -1 -1
        ]
    , edge =
        [ -- Back & Top
          Vector.vector -1 0 1
        , -- Left & Top
          Vector.vector 0 -1 1
        , -- Front & Top
          Vector.vector 1 0 1
        , -- Right & Top
          Vector.vector 0 1 1
        , -- Back & Down
          Vector.vector -1 0 -1
        , -- Left & Down
          Vector.vector 0 -1 -1
        , -- Front & Down
          Vector.vector 1 0 -1
        , -- Right & Down
          Vector.vector 0 1 -1
        , -- Front & Left
          Vector.vector 1 -1 0
        , -- Front & Right
          Vector.vector 1 1 0
        , -- Back & Right
          Vector.vector -1 1 0
        , -- Back & Left
          Vector.vector -1 -1 0
        ]
    , center =
        [ -- 0 -> Top
          Vector.vector 0 0 1
        , -- 1 -> Left
          Vector.vector 0 -1 0
        , -- 2 -> Front
          Vector.vector 1 0 0
        , -- 3 -> Right
          Vector.vector 0 1 0
        , -- 4 -> Back
          Vector.vector -1 0 0
        , -- 5 -> Down
          Vector.vector 0 0 -1
        ]
    }


positionsInGlobalRotation : GlobalRotation -> Vector -> Vector
positionsInGlobalRotation =
    Quaternion.rotate


centerOfFrame : { x : Float, y : Float }
centerOfFrame =
    { x = 300, y = 300 }


displayCoefficient : Float
displayCoefficient =
    105


perspectiveCoefficient : Float
perspectiveCoefficient =
    15


displayedPosition : Vector -> { x : Float, y : Float }
displayedPosition v =
    let
        x =
            Vector.getX v

        y =
            Vector.getY v

        z =
            Vector.getZ v
    in
    { x = (displayCoefficient + x * perspectiveCoefficient) * y + centerOfFrame.x
    , y = (displayCoefficient + x * perspectiveCoefficient) * -z + centerOfFrame.y
    }


type SelectedObject
    = Edge Int
    | Corner Int
    | Center Side


stringOfSelectedObject : SelectedObject -> String
stringOfSelectedObject so =
    case so of
        Edge n ->
            "Edge of " ++ String.fromInt n

        Corner n ->
            "Corner of " ++ String.fromInt n

        Center side ->
            "Center of " ++ stringOfSide side


mouseOveredObject : GlobalRotation -> { x : Float, y : Float } -> Maybe SelectedObject
mouseOveredObject q pos =
    let
        selectedObjects =
            [ initialPositions.corner |> List.indexedMap (\i vCenter -> ( vCenter, Corner i ))
            , initialPositions.edge |> List.indexedMap (\i vCenter -> ( vCenter, Edge i ))
            , initialPositions.center |> List.indexedMap (\i vCenter -> ( vCenter, Center (sideOfNumber i) ))
            ]
                |> List.concat
                |> List.map
                    (\( vCenter, selectedObject ) ->
                        let
                            vCornerOfObject =
                                List.map (\v -> Vector.add v vCenter)
                                    [ Vector.vector -0.5 -0.5 -0.5
                                    , Vector.vector -0.5 -0.5 0.5
                                    , Vector.vector -0.5 0.5 -0.5
                                    , Vector.vector -0.5 0.5 0.5
                                    , Vector.vector 0.5 -0.5 -0.5
                                    , Vector.vector 0.5 -0.5 0.5
                                    , Vector.vector 0.5 0.5 -0.5
                                    , Vector.vector 0.5 0.5 0.5
                                    ]

                            vRotatedCornerOfObject =
                                List.map (positionsInGlobalRotation q) vCornerOfObject

                            pDisplayedCornerOfObject =
                                List.map displayedPosition vRotatedCornerOfObject
                        in
                        if isInConvexArea pDisplayedCornerOfObject pos then
                            Just ( Vector.getX (positionsInGlobalRotation q vCenter), selectedObject )

                        else
                            Nothing
                    )
                |> List.filterMap identity
    in
    List.foldl
        (\( x, selectedObject ) acc ->
            case acc of
                Nothing ->
                    Just { x = x, object = selectedObject }

                Just result ->
                    if result.x > x then
                        acc

                    else
                        Just { x = x, object = selectedObject }
        )
        Nothing
        selectedObjects
        |> Maybe.map .object



-- View


cubeView : GlobalRotation -> Cube -> Maybe ( Side, Direction, Float ) -> Entity coordinate
cubeView q cube rotatingSide =
    ofCube cube |> entityOfCubeColors rotatingSide |> globalRotateWithEulerAngles (toEulerAngles q)
