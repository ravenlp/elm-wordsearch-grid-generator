module Directions exposing 
  ( Direction,
  next,
  fromInt,
  toInt,
  random
  )
import Random exposing (Generator, int, map)

type Direction
  = E
  | SE
  | S
  | SW
  | W
  | NW
  | N
  | NE

next: Direction -> Direction
next currentDir= 
  case currentDir of
    E ->  SE
    SE -> S
    S ->  SW
    SW -> W
    W ->  NW
    NW -> N
    N ->  NE
    NE -> E

fromInt: Int -> Direction
fromInt index =
  case index of
    0 -> E
    1 -> SE
    2 -> S 
    3 -> SW
    4 -> W 
    5 -> NW
    6 -> N 
    _ -> NE 

toInt: Direction -> (Int, Int)
toInt dir =  
  case dir of
    E ->  (1,0)
    SE -> (1,1)
    S ->  (0,1)
    SW -> (-1,1)
    W ->  (-1,0)
    NW -> (-1,-1)
    N ->  (0,-1)
    NE -> (1,-1)

random: Generator Direction
random = map fromInt (int 0 7)