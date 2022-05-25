module Main exposing (main)

import Angle
import Browser
import Browser.Events
import Camera3d
import Color
import Cube exposing (..)
import CubeView exposing (cubeView, initGlobalRotation, rotateAnimationTime, updateGlobalRotation)
import Direction3d
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Json.Decode
import Length
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d
import Scene3d exposing (..)
import Time
import Viewpoint3d


tickPeriod : Float
tickPeriod =
    20


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


type Mode
    = RotateMode { x : Int, y : Int }
    | NormalMode


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { mode : Mode
    , globalRotation : CubeView.GlobalRotation
    , cube : Cube
    , rotatingSide : Maybe ( Side, Float )
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model NormalMode (initGlobalRotation ()) (Cube.init ()) Nothing
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onMouseDown (decodeMouse MouseDown)
        , Browser.Events.onMouseMove (decodeMouse MouseMove)
        , Browser.Events.onMouseUp (Json.Decode.succeed MouseUp)
        , Time.every tickPeriod Tick
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
    | Tick Time.Posix


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown mouse ->
            let
                { x, y } =
                    Point2d.toPixels mouse
            in
            { model | mode = RotateMode { x = round x, y = round y } }

        MouseMove mouse ->
            case model.mode of
                RotateMode { x, y } ->
                    let
                        newPoint =
                            Point2d.toPixels mouse
                    in
                    { model
                        | mode = RotateMode { x = round newPoint.x, y = round newPoint.y }
                        , globalRotation = updateGlobalRotation { dx = round newPoint.x - x, dy = round newPoint.y - y } model.globalRotation
                    }

                _ ->
                    model

        MouseUp ->
            case model.mode of
                RotateMode _ ->
                    { model | mode = NormalMode }

                _ ->
                    model

        RotateCube side ->
            { model | rotatingSide = Just ( side, 0 ) }

        Reset ->
            { model | cube = Cube.init () }

        Tick _ ->
            case model.rotatingSide of
                Just ( side, ratio ) ->
                    if ratio >= 1 then
                        { model | cube = Cube.rotate side model.cube, rotatingSide = Nothing }

                    else
                        { model | rotatingSide = Just ( side, ratio + (rotateAnimationTime |> toFloat |> (/) tickPeriod) ) }

                Nothing ->
                    model


view : Model -> Html Msg
view { cube, rotatingSide, globalRotation } =
    let
        isButtonDisabled =
            rotatingSide == Nothing |> not |> disabled
    in
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
                cubeView globalRotation cube rotatingSide
                    |> List.singleton
            }
        , button [ onClick (RotateCube Top), isButtonDisabled ] [ text "Top" ]
        , button [ onClick (RotateCube Left), isButtonDisabled ] [ text "Left" ]
        , button [ onClick (RotateCube Front), isButtonDisabled ] [ text "Front" ]
        , button [ onClick (RotateCube Right), isButtonDisabled ] [ text "Right" ]
        , button [ onClick (RotateCube Back), isButtonDisabled ] [ text "Back" ]
        , button [ onClick (RotateCube Down), isButtonDisabled ] [ text "Down" ]
        , button [ onClick Reset, isButtonDisabled ] [ text "reset" ]
        ]
