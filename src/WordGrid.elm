module WordGrid exposing 
  ( WordGrid
  , create
  , fromList
  , toList
  , toLists
  , getRow
  , getSize
  , map
  )

import Array exposing (Array)

-- Grid

type WordGrid a
  = WordGrid 
    { size : Int,
      data : Array a
    }
    

-- INIT

create : Int -> a -> WordGrid a
create n seed = 
  WordGrid 
    { 
      size= n,
      data= Array.repeat (n * n) seed
    }

-- DISPLAY


-- TODO MAYBE
fromList: List a -> Int -> WordGrid a
fromList list size_ = 
  WordGrid {
    size = size_,
    data = Array.fromList (List.take (size_*size_) list)
    }

toList: WordGrid a -> List a
toList (WordGrid {data})  = 
  Array.toList data

toLists: WordGrid a -> List (List a)
toLists = 
  let 
    toLists_ : Int -> WordGrid a -> List (List a)
    toLists_ i g = 
      if i < getSize g
      then (getRow g i) :: (toLists_ (i + 1) g)
      else []
  in
    toLists_ 0 

-- GET & SET

getSize: WordGrid a -> Int
getSize (WordGrid {size}) =
  size

getRow: (WordGrid a) -> Int -> List a
getRow (WordGrid {size, data}) row = 
  let
    list = Array.toList data
    rest = List.drop (row * size) list
  in 
    List.take size rest

map: (a -> b) -> WordGrid a -> WordGrid b
map f (WordGrid w) =
  WordGrid {
    size = w.size
  , data = Array.map f w.data
  }

--setLetter: WordGrid a -> WordGrid b
