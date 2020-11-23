module Cube exposing (Color(..), Cube, init, ofColor)

import Color as ObjColor


type Color
    = White
    | Orange
    | Green
    | Red
    | Blue
    | Yellow


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


type alias Side =
    List Color


type alias Cube =
    List Side


initialSide : Color -> Side
initialSide color =
    [ color, color, color, color, color, color, color, color, color ]


init : () -> Cube
init _ =
    [ initialSide White
    , initialSide Orange
    , initialSide Green
    , initialSide Red
    , initialSide Blue
    , initialSide Yellow
    ]
