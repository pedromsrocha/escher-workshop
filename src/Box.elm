module Box exposing (..)

import Vector exposing (..)

type alias Box = 
  { a : Vector
  , b : Vector
  , c : Vector }

-- Exercise 1 

turnBox : Box -> Box 
turnBox { a, b, c } = 
  { a = add a b
  , b = c 
  , c =  neg b}


-- Exercise 2 

flipBox : Box -> Box 
flipBox {a, b, c} = 
  { a = add a b
  , b = neg b 
  , c = c}

-- Exercise 3 
tossBox : Box -> Box 
tossBox {a, b, c} = 
  { a = add a (scale 0.5 (add b c))
  , b = scale 0.5 (add b c)
  , c = scale 0.5 (sub c b)}

-- Exercise 4 


shrunkBox : Box -> Box 
shrunkBox {a, b, c} =
  { a = a
  , b = b
  , c = scale 0.5 c}

upBox : Box -> Box 
upBox {a, b, c} = 
  { a = add a (scale 0.5 c)
  , b = b
  , c = c}