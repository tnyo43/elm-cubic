module Main exposing (main)

import Angle
import Block3d
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d


main : Html msg
main =
    Scene3d.unlit
        { dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 5 2 3
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 40
                }
        , clipDepth = Length.meters 3
        , background = Scene3d.transparentBackground
        , entities =
            [ Scene3d.block
                (Material.color Color.black)
                (Block3d.with
                    { x1 = Length.meters 1
                    , x2 = Length.meters 0
                    , y1 = Length.meters 1
                    , y2 = Length.meters 0
                    , z1 = Length.meters 1
                    , z2 = Length.meters 0
                    }
                )
            ]
        }
