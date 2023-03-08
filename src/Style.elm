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

strokeColorStyle : StyleColor -> Style -> Style 
strokeColorStyle color {stroke, fill} = 
  case stroke of 
   Nothing -> {stroke= Nothing
              , fill= fill}
   Just x ->  {stroke = Just {strokeWidth = x.strokeWidth 
                              , strokeColor = color}
              , fill = fill}