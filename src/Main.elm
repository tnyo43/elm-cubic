module Main exposing (main)

import Angle
import Axis3d
import Browser
import Browser.Events
import Camera3d
import Color
import Cube exposing (..)
import CubeView
import Direction3d
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode
import Length
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d
import Scene3d exposing (..)
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
    = RotateMode
        { from : Point2d Pixels ScreenCoordinates
        , to : Point2d Pixels ScreenCoordinates
        }
    | NormalMode


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { mode : Mode
    , rotation : Vector2d Pixels ScreenCoordinates
    , data : Data
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model NormalMode Vector2d.zero (Cube.init ())
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
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
    | RotateCube Side
    | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown mouse ->
            { model | mode = RotateMode { from = mouse, to = mouse } }

        MouseMove mouse ->
            case model.mode of
                RotateMode { from } ->
                    { model
                        | mode = RotateMode { from = from, to = mouse }
                    }

                _ ->
                    model

        MouseUp ->
            case model.mode of
                RotateMode { from, to } ->
                    { model
                        | mode = NormalMode
                        , rotation = Vector2d.from from to |> Vector2d.plus model.rotation
                    }

                _ ->
                    model

        RotateCube side ->
            let
                data_ =
                    Cube.rotate side model.data
            in
            { model | data = data_ }

        Reset ->
            { model | data = Cube.init () }


rotate : Vector2d Pixels ScreenCoordinates -> Entity coordinates -> Entity coordinates
rotate rotation e =
    let
        { x, y } =
            Vector2d.toPixels rotation
    in
    rotateAround Axis3d.z (Angle.radians (x / 200)) e
        |> rotateAround Axis3d.y (Angle.radians (y / 200))


view : Model -> Html Msg
view { rotation, mode, data } =
    div []
        [ Scene3d.unlit
            { dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
            , camera =
                Camera3d.perspective
                    { viewpoint =
                        Viewpoint3d.lookAt
                            { focalPoint = Point3d.origin
                            , eyePoint = Point3d.meters 9 0 3
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
                            RotateMode { from, to } ->
                                Vector2d.from from to |> Vector2d.plus rotation

                            NormalMode ->
                                rotation
                in
                CubeView.ofEntity data
                    |> rotate rot
                    |> List.singleton
            }
        , button [ onClick (RotateCube Top) ] [ text "Top" ]
        , button [ onClick (RotateCube Left) ] [ text "Left" ]
        , button [ onClick (RotateCube Front) ] [ text "Front" ]
        , button [ onClick (RotateCube Right) ] [ text "Right" ]
        , button [ onClick (RotateCube Back) ] [ text "Back" ]
        , button [ onClick (RotateCube Down) ] [ text "Down" ]
        , button [ onClick Reset ] [ text "reset" ]
        ]
