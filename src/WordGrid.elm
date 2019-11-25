module WordGrid exposing 
  ( WordGrid
  , create
  , toList
  , toLists
  , fromList
  , getRow
  , getSize
  , map
  )

import Array exposing (Array)

-- Grid

type WordGrid
  = WordGrid 
    { size : Int,
      data : Array Char
    }
    

-- INIT

create : Int -> Char -> WordGrid
create n seed = 
  WordGrid 
    { 
      size= n,
      data= Array.repeat (n * n) seed
    }

-- DISPLAY


-- TODO MAYBE
fromList: List Char -> Int -> WordGrid
fromList list size_ = 
  WordGrid {
    size = size_,
    data = Array.fromList (List.take (size_*size_) list)
    }

toList: WordGrid -> List Char
toList (WordGrid {data})  = 
  Array.toList data

toLists: WordGrid -> List (List Char)
toLists = 
  let 
    toLists_ : Int -> WordGrid -> List (List Char)
    toLists_ i g = 
      if i < getSize g
      then (getRow g i) :: (toLists_ (i + 1) g)
      else []
  in
    toLists_ 0 

-- GET & SET

getSize: WordGrid -> Int
getSize (WordGrid {size}) =
  size

getRow: (WordGrid) -> Int -> List Char
getRow (WordGrid {size, data}) row = 
  let
    list = Array.toList data
    rest = List.drop (row * size) list
  in 
    List.take size rest

map: (Char -> Char) -> WordGrid -> WordGrid
map f (WordGrid w) =
  WordGrid {
    size = w.size
  , data = Array.map f w.data
  }

--setLetter: WordGrid -> WordGrid b
