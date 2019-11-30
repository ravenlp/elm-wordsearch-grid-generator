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
      data : Array (Maybe Char)
    }
    

-- INIT

create : Int -> (Maybe Char) -> WordGrid
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
    data = Array.fromList (List.take (size_*size_) (List.map (\e -> Just e) list))
    }

toList: WordGrid -> List (Maybe Char)
toList (WordGrid {data})  = 
  Array.toList data

toLists: WordGrid -> List (List (Maybe Char))
toLists = 
  let 
    toLists_ : Int -> WordGrid -> List (List (Maybe Char))
    toLists_ i g = 
      if i < getSize g
      then Array.toList(getRow g i) :: (toLists_ (i + 1) g)
      else []
  in
    toLists_ 0 

-- GET & SET

getSize: WordGrid -> Int
getSize (WordGrid {size}) =
  size

getRow: WordGrid -> Int -> Array (Maybe Char)
getRow (WordGrid {size, data}) row = 
    Array.slice (row * size) (row * size + size) data

getCell: (Int, Int) -> WordGrid -> Maybe Char
getCell (x, y) grid = 
  let 
    row = getRow grid x
    result = Array.get y row
  in
  case result of
    Just a -> a
    Nothing -> Nothing


map: ((Maybe Char) -> (Maybe Char)) -> WordGrid -> WordGrid
map f (WordGrid w) =
  WordGrid {
    size = w.size
  , data = Array.map f w.data
  }

--setLetter: WordGrid -> WordGrid b
