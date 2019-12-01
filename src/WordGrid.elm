module WordGrid exposing 
  ( WordGrid
  , create
  , toList
  , toLists
  , fromList
  , getRow
  , getSize
  , getCell
  , setCell
  , map
  )

import Array exposing (Array)

-- Grid

type alias WordGrid =  
    { size : Int,
      data : Array (Maybe Char)
    }
    

-- INIT

create : Int -> (Maybe Char) -> WordGrid
create n seed = 
  WordGrid n (Array.repeat (n * n) seed)

-- DISPLAY


-- TODO MAYBE
fromList: List Char -> Int -> WordGrid
fromList list size_ = 
  WordGrid size_ (Array.fromList (List.take (size_*size_) (List.map (\e -> Just e) list)))

toList: WordGrid -> List (Maybe Char)
toList grid = 
  Array.toList grid.data

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
getSize grid =
  grid.size

getRealPosition: (Int, Int) -> WordGrid -> Int
getRealPosition (x, y) grid=
  x * grid.size + y


getRow: WordGrid -> Int -> Array (Maybe Char)
getRow grid row = 
    Array.slice (row * grid.size) (row * grid.size + grid.size) grid.data

getCell: WordGrid -> (Int, Int) ->  Maybe Char
getCell grid (x, y) = 
  let 
    row = getRow grid x
    result = Array.get y row
  in
  case result of
    Just a -> a
    Nothing -> Nothing

setCell: WordGrid -> (Int, Int) -> Maybe Char -> Maybe WordGrid
setCell grid (x, y) new =
  case new of
    Nothing -> Just grid
    Just letter -> 
      let
        cell = getCell grid (x, y)
        position = getRealPosition (x,y) grid
        _ = Debug.log "----- ADDING " (position, new)
      in 
        case cell of
          Nothing -> Just (WordGrid grid.size (Array.set position new grid.data))
          Just _ -> Nothing



map: ((Maybe Char) -> (Maybe Char)) -> WordGrid -> WordGrid
map f grid =
  WordGrid grid.size (Array.map f grid.data)

--setLetter: WordGrid -> WordGrid b
