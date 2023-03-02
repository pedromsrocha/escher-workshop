module Style exposing (..)

type alias StyleColor = String

type alias StrokeStyle = 
  { strokeWidth : Float 
  , strokeColor : StyleColor }

type alias FillStyle = 
  { fillColor : StyleColor }

type alias Style = 
  { stroke : Maybe StrokeStyle 
  , fill : Maybe FillStyle }