module Main exposing (main)

import Angle exposing (Angle)
import Axis3d
import Block3d
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Json.Decode
import Length
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d
import Scene3d exposing (..)
import Scene3d.Material as Material
import Vector2d exposing (Vector2d)
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


type Mode
    = Rotate
        { from : Point2d Pixels ScreenCoordinates
        , to : Point2d Pixels ScreenCoordinates
        }
    | Nothing


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { mode : Mode
    , rotation : Vector2d Pixels ScreenCoordinates
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Vector2d.zero
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onMouseDown (decodeMouse MouseDown)
        , Browser.Events.onMouseMove (decodeMouse MouseMove)
        , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
        ]


decodeMouse : (Point2d Pixels ScreenCoordinates -> Msg) -> Json.Decode.Decoder Msg
decodeMouse msg =
    Json.Decode.map2 (\x y -> msg (Point2d.pixels x y))
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)


type Msg
    = MouseDown (Point2d Pixels ScreenCoordinates)
    | MouseMove (Point2d Pixels ScreenCoordinates)
    | MouseUp


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown mouse ->
            { model | mode = Rotate { from = mouse, to = mouse } }

        MouseMove mouse ->
            case model.mode of
                Rotate { from } ->
                    { model
                        | mode = Rotate { from = from, to = mouse }
                    }

                _ ->
                    model

        MouseUp ->
            case model.mode of
                Rotate { from, to } ->
                    { model
                        | mode = Nothing
                        , rotation = Vector2d.from from to |> Vector2d.plus model.rotation
                    }

                _ ->
                    model


rotate : Vector2d Pixels ScreenCoordinates -> Entity coordinates -> Entity coordinates
rotate rotation e =
    let
        { x, y } =
            Vector2d.toPixels rotation
    in
    rotateAround Axis3d.z (Angle.radians (x / 200)) e
        |> rotateAround Axis3d.y (Angle.radians (y / 200))


view : Model -> Html msg
view { rotation, mode } =
    Scene3d.unlit
        { dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 4 0 2
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 40
                }
        , clipDepth = Length.meters 3.4
        , background = Scene3d.backgroundColor Color.grey
        , entities =
            let
                rot =
                    case mode of
                        Rotate { from, to } ->
                            Vector2d.from from to |> Vector2d.plus rotation

                        Nothing ->
                            rotation
            in
            [ Scene3d.block
                (Material.color Color.black)
                (Block3d.with
                    { x1 = Length.meters 0.5
                    , x2 = Length.meters -0.5
                    , y1 = Length.meters 0.5
                    , y2 = Length.meters -0.5
                    , z1 = Length.meters 0.5
                    , z2 = Length.meters -0.5
                    }
                )
            , Scene3d.quad (Material.color Color.green)
                (Point3d.meters -0.45 -0.45 0.51)
                (Point3d.meters 0.45 -0.45 0.51)
                (Point3d.meters 0.45 0.45 0.51)
                (Point3d.meters -0.45 0.45 0.51)
            , Scene3d.quad (Material.color Color.blue)
                (Point3d.meters -0.45 -0.45 -0.51)
                (Point3d.meters 0.45 -0.45 -0.51)
                (Point3d.meters 0.45 0.45 -0.51)
                (Point3d.meters -0.45 0.45 -0.51)
            , Scene3d.quad (Material.color Color.white)
                (Point3d.meters 0.51 -0.45 0.45)
                (Point3d.meters 0.51 -0.45 -0.45)
                (Point3d.meters 0.51 0.45 -0.45)
                (Point3d.meters 0.51 0.45 0.45)
            , Scene3d.quad (Material.color Color.yellow)
                (Point3d.meters -0.51 -0.45 0.45)
                (Point3d.meters -0.51 -0.45 -0.45)
                (Point3d.meters -0.51 0.45 -0.45)
                (Point3d.meters -0.51 0.45 0.45)
            , Scene3d.quad (Material.color Color.red)
                (Point3d.meters -0.45 -0.51 0.45)
                (Point3d.meters -0.45 -0.51 -0.45)
                (Point3d.meters 0.45 -0.51 -0.45)
                (Point3d.meters 0.45 -0.51 0.45)
            , Scene3d.quad (Material.color Color.orange)
                (Point3d.meters -0.45 0.51 0.45)
                (Point3d.meters -0.45 0.51 -0.45)
                (Point3d.meters 0.45 0.51 -0.45)
                (Point3d.meters 0.45 0.51 0.45)
            ]
                |> group
                |> rotate rot
                |> List.singleton
        }
