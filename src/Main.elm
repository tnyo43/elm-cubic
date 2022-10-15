module Main exposing (main)

import Angle
import Browser
import Browser.Events
import Camera3d
import Color
import Cube exposing (..)
import CubeView
    exposing
        ( Rotating(..)
        , SelectedObject
        , cubeRotateByMouse
        , cubeView
        , initGlobalRotation
        , mouseOveredObject
        , rotateAnimationTime
        , updateGlobalRotation
        )
import Direction3d
import Html exposing (Attribute, Html, button, div, table, td, text, tr)
import Html.Attributes exposing (disabled, tabindex)
import Html.Events exposing (keyCode, on, onClick)
import Json.Decode
import Length
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d
import Scene3d exposing (..)
import Time
import Utils exposing (toIntPoint2d)
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
    = NormalMode
    | GlobalRotateMode { x : Int, y : Int }
    | CubeRotateMode { x : Float, y : Float } SelectedObject


type ScreenCoordinates
    = ScreenCoordinates


type alias Model =
    { mode : Mode
    , globalRotation : CubeView.GlobalRotation
    , cube : Cube
    , rotating : Maybe ( Rotating, Direction, Float )
    , mousePosition : { x : Float, y : Float }
    , shiftPush : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model NormalMode (initGlobalRotation ()) (Cube.init ()) Nothing { x = 0, y = 0 } False
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onMouseDown (decodeMouse MouseDown)
        , Browser.Events.onMouseMove (decodeMouse MouseMove)
        , Browser.Events.onMouseUp (decodeMouse MouseUp)
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
    | MouseUp (Point2d Pixels ScreenCoordinates)
    | OnKeyDown Int
    | OnKeyUp Int
    | RotateCube Rotating Direction
    | Reset
    | Tick Time.Posix


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown mouse ->
            let
                { x, y } =
                    Point2d.toPixels mouse |> toIntPoint2d
            in
            if model.shiftPush then
                case mouseOveredObject model.globalRotation { x = toFloat x, y = toFloat y } of
                    Just selectedObject ->
                        { model | mode = CubeRotateMode { x = toFloat x, y = toFloat y } selectedObject }

                    Nothing ->
                        { model | mode = GlobalRotateMode { x = x, y = y } }

            else
                { model | mode = GlobalRotateMode { x = x, y = y } }

        MouseMove mouse ->
            case model.mode of
                GlobalRotateMode { x, y } ->
                    let
                        newPoint =
                            Point2d.toPixels mouse |> toIntPoint2d
                    in
                    { model
                        | mode = GlobalRotateMode { x = newPoint.x, y = newPoint.y }
                        , globalRotation = updateGlobalRotation { dx = newPoint.x - x, dy = newPoint.y - y } model.globalRotation
                        , mousePosition = Point2d.toPixels mouse
                    }

                _ ->
                    { model | mousePosition = Point2d.toPixels mouse }

        MouseUp mouse ->
            case model.mode of
                CubeRotateMode from selectedObject ->
                    let
                        to =
                            Point2d.toPixels mouse
                                |> toIntPoint2d
                                |> (\{ x, y } -> { x = toFloat x, y = toFloat y })
                    in
                    case cubeRotateByMouse model.globalRotation from to selectedObject of
                        Just { rotateTarget, direction } ->
                            case rotateTarget of
                                Side side ->
                                    { model
                                        | mode = NormalMode
                                        , rotating = Just ( Side side, direction, 0 )
                                    }

                                Middle axis ->
                                    { model
                                        | mode = NormalMode
                                        , rotating = Just ( Middle axis, direction, 0 )
                                    }

                        Nothing ->
                            { model | mode = NormalMode }

                _ ->
                    { model | mode = NormalMode }

        OnKeyDown code ->
            case code of
                16 ->
                    -- Shift Key を押したとき
                    { model | shiftPush = True }

                _ ->
                    model

        OnKeyUp code ->
            case code of
                16 ->
                    -- Shift Key を上げたとき
                    { model | shiftPush = False }

                _ ->
                    model

        RotateCube (Side side) direction ->
            { model | rotating = Just ( Side side, direction, 0 ) }

        RotateCube (Middle axis) direction ->
            { model | rotating = Just ( Middle axis, direction, 0 ) }

        Reset ->
            { model | cube = Cube.init () }

        Tick _ ->
            case model.rotating of
                Just ( Side side, direction, ratio ) ->
                    if ratio >= 1 then
                        { model | cube = Cube.rotateSide side direction model.cube, rotating = Nothing }

                    else
                        { model | rotating = Just ( Side side, direction, ratio + (rotateAnimationTime |> toFloat |> (/) tickPeriod) ) }

                Just ( Middle axis, direction, ratio ) ->
                    if ratio >= 1 then
                        { model | cube = Cube.rotateMiddle axis direction model.cube, rotating = Nothing }

                    else
                        { model | rotating = Just ( Middle axis, direction, ratio + (rotateAnimationTime |> toFloat |> (/) tickPeriod) ) }

                _ ->
                    model


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.Decode.map tagger keyCode)


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.Decode.map tagger keyCode)


view : Model -> Html Msg
view { cube, rotating, globalRotation, mousePosition, mode } =
    let
        isButtonDisabled =
            rotating == Nothing |> not |> disabled

        selected =
            case mode of
                CubeRotateMode _ selectedObject ->
                    Just selectedObject

                _ ->
                    mouseOveredObject globalRotation mousePosition
    in
    div [ onKeyUp OnKeyUp, onKeyDown OnKeyDown, tabindex 0 ]
        [ Scene3d.unlit
            { dimensions = ( Pixels.pixels 600, Pixels.pixels 600 )
            , camera =
                Camera3d.perspective
                    { viewpoint =
                        Viewpoint3d.lookAt
                            { focalPoint = Point3d.origin
                            , eyePoint = Point3d.meters 9 0 0
                            , upDirection = Direction3d.positiveZ
                            }
                    , verticalFieldOfView = Angle.degrees 35
                    }
            , clipDepth = Length.meters 3.4
            , background = Scene3d.backgroundColor Color.grey
            , entities =
                cubeView globalRotation cube rotating selected
                    |> List.singleton
            }
        , table []
            [ tr []
                [ td [] [ button [ onClick (RotateCube (Side Top) CW), isButtonDisabled ] [ text "Top(CW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Left) CW), isButtonDisabled ] [ text "Left(CW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Front) CW), isButtonDisabled ] [ text "Front(CW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Right) CW), isButtonDisabled ] [ text "Right(CW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Back) CW), isButtonDisabled ] [ text "Back(CW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Bottom) CW), isButtonDisabled ] [ text "Bottom(CW)" ] ]
                ]
            , tr
                []
                [ td [] [ button [ onClick (RotateCube (Side Top) CCW), isButtonDisabled ] [ text "Top(CCW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Left) CCW), isButtonDisabled ] [ text "Left(CCW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Front) CCW), isButtonDisabled ] [ text "Front(CCW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Right) CCW), isButtonDisabled ] [ text "Right(CCW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Back) CCW), isButtonDisabled ] [ text "Back(CCW)" ] ]
                , td [] [ button [ onClick (RotateCube (Side Bottom) CCW), isButtonDisabled ] [ text "Bottom(CCW)" ] ]
                ]
            , tr []
                [ td [] [ button [ onClick (RotateCube (Middle X) CW), isButtonDisabled ] [ text "X(CW)" ] ]
                , td [] [ button [ onClick (RotateCube (Middle Y) CW), isButtonDisabled ] [ text "Y(CW)" ] ]
                , td [] [ button [ onClick (RotateCube (Middle Z) CW), isButtonDisabled ] [ text "Z(CW)" ] ]
                ]
            , tr
                []
                [ td [] [ button [ onClick (RotateCube (Middle X) CCW), isButtonDisabled ] [ text "X(CCW)" ] ]
                , td [] [ button [ onClick (RotateCube (Middle Y) CCW), isButtonDisabled ] [ text "Y(CCW)" ] ]
                , td [] [ button [ onClick (RotateCube (Middle Z) CCW), isButtonDisabled ] [ text "Z(CCW)" ] ]
                ]
            ]
        , button [ onClick Reset, isButtonDisabled ] [ text "reset" ]
        ]
