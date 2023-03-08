module Picture exposing (..)

import Box exposing (..)
import Shape exposing (..)
import Style exposing (..)
import List exposing (map)
import Svg.Attributes exposing (stroke)

type alias Rendering = List (Shape, Style)

type alias Picture = Box -> Rendering  

blank : Picture 
blank _ = []


-- color scheme 
color1 : StyleColor 
color1 = "Red"

color2 : StyleColor 
color2 = "Green"

color3 : StyleColor 
color3 = "Blue"

-- set the stroke color of a picture 
strokeColorPic : StyleColor -> Picture -> Picture 
strokeColorPic color p = p >> map (\(x,y) -> (x, strokeColorStyle color y))


-- Exercise 1

turn : Picture -> Picture
turn p = turnBox >> p

-- Entirely optional bonus exercise:
times : Int -> (a -> a) -> (a -> a)
times n fn = 
  if n == 0 then identity 
  else fn >> (times (n - 1) fn)

turns : Int -> Picture -> Picture
turns n = times n turn 

-- Exercise 2

flip : Picture -> Picture 
flip p = flipBox >> p 

-- Exercise 3

toss : Picture -> Picture 
toss p = tossBox >> p 

-- Exercise 4

aboveRatio : Int -> Int -> Picture -> Picture -> Picture 
aboveRatio m n p1 p2 = 
  \box -> 
    let 
      f = toFloat m / toFloat (m + n)
      (b1, b2) = splitVertically f box
    in 
      (p1 b1) ++ (p2 b2) 

above : Picture -> Picture -> Picture 
above = aboveRatio 1 1  

-- Exercise 5

besideRatio : Int -> Int -> Picture -> Picture -> Picture 
besideRatio m n p1 p2 = 
  \box -> 
    let 
      f = toFloat m / toFloat (m + n)
      (b1, b2) = splitHorizontally f box
    in 
      (p1 b1) ++ (p2 b2) 

beside : Picture -> Picture -> Picture 
beside = besideRatio 1 1 

-- Exercise 6

quartet : Picture -> Picture -> Picture -> Picture -> Picture
quartet nw ne sw se = 
  above (beside nw ne)
        (beside sw se)

-- Exercise 7

nonet : Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture 
nonet nw nm ne mw mm me sw sm se = 
  let 
    row w m e = besideRatio 1 2 w (beside m e)
    column n m s = aboveRatio 1 2 n (above m s)
  in 
    column (row nw nm ne)
           (row mw mm me)
           (row sw sm se)

-- Exercise 8

over : Picture -> Picture -> Picture 
over p1 p2 = 
  \box -> p1 box ++ p2 box

overall : List Picture -> Picture 
overall ps = 
  \box -> List.concatMap (\p -> p box) ps

-- Exercise 9

ttile : Picture -> Picture
ttile fish = 
  let 
    fishN = fish |> toss |> flip |> strokeColorPic color1  
    fishE = fishN |> turn |> turn |> turn |> strokeColorPic color3 
  in 
    over (fish |> strokeColorPic color2) (over fishN fishE) 

-- Exercise 10

utile : Picture -> Picture 
utile fish = 
  let 
    fishN = fish |> toss |> flip |> strokeColorPic color1  
    fishW = turn fishN |> strokeColorPic color3
    fishS = turn fishW |> strokeColorPic color1 
    fishE = turn fishS |> strokeColorPic color2
  in 
    over fishN (over fishW (over fishS fishE))

-- Exercise 11

side : Int -> Picture -> Picture 
side n fish = 
  if n == 0 then blank 
  else 
    let
      s = side (n - 1) fish 
      t = ttile fish 
    in 
      quartet s s (turn t) t  

-- Exercise 12

corner : Int -> Picture -> Picture 
corner n fish =
  if n == 0 then blank 
  else  
    let 
      c = corner (n - 1) fish 
      s = side (n - 1) fish 
    in 
      quartet c s (turn s) (utile fish)

-- Exercise 13

squareLimit : Int -> Picture -> Picture
squareLimit n fish = 
  let 
    c = corner n fish 
    s = side n fish 
    nw = c 
    nm = s 
    ne = c |> turn |> turn |> turn
    mw = s |> turn
    mm = utile fish 
    me = s |> turn |> turn |> turn
    sw = c |> turn
    sm = s |> turn |> turn
    se = c |> turn |> turn
  in
    nonet nw nm ne mw mm me sw sm se    
