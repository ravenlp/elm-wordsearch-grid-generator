module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Random
import Random.String
import Random.Char
import Random.List
import Maybe.Extra


import Html exposing (Html, button, div, text, ul, li)
import Html.Attributes as Attribute
import Html.Events exposing (onClick)


import WordGrid exposing (WordGrid)
import Directions exposing (Direction)



-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


-- MODEL



 
type alias Model = 
  { grid: WordGrid
  , size: Int
  , counter: Int
  , wordList: List String
  , wordsToFind: List String
  }
  

type alias Flags =
  {}

type alias Job =
  ( String, ( Int, Int ), Direction )


-- INIT

init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    test = WordGrid.create 10 Nothing
    model_ = 
      { grid = test
      , size = 10
      , counter= 10
      , wordList= getWordList
      , wordsToFind = []
      }
    
    _ = Debug.log "WordGrid getRow" (WordGrid.getRow test 0)
    _ = Debug.log "WordGrid toList" (WordGrid.toList test)
    _ = Debug.log "WordGrid size" (WordGrid.getSize test)
    _ = Debug.log "WordGrid toLists" (WordGrid.toLists test)
    _ = Debug.log "WordGrid" (WordGrid.getRow test 1)
  in
    (model_, Cmd.none)
  


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- UPDATE


type Msg
  = NewValue Int
  | GetWordList
  | GenerateGame
  | FillGridWithRandomChars 
  | FillGridWithRandomChars_ String
  | PickRandomWord
  | PickRandomWordResult (Maybe String, List String)
  | PlaceSelectedWord Job



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewValue number -> 
      let 
        _ = Debug.log "Number" number
      in
        (model , Cmd.none)

    GetWordList ->
      let 
        _ = Debug.log "GetWordList" 1
      in
        ({ model | wordList=  getWordList}, Cmd.none)

    FillGridWithRandomChars_ randomWord->
      let 
        _ = Debug.log "GetWordList" randomWord
        grid_ = WordGrid.fromList (String.toList randomWord) (WordGrid.getSize model.grid)
      in
      ({model | grid = grid_}, Cmd.none)

    FillGridWithRandomChars ->
      ( model,
        model.grid
          |> WordGrid.getSize
          |> \x -> x * x
          |> randomString
          |> Random.generate FillGridWithRandomChars_ )
         
    GenerateGame ->
      let
        grid_ = generateGame model
      in
      ({ model | grid = grid_ }, Cmd.none)

    PickRandomWord -> 
      let 
        list_ = List.filter (\s -> (String.length s) <= model.size) model.wordList
        _ = Debug.log "new word list" list_
      in
      ({ model | wordList = list_ }, Random.generate PickRandomWordResult (randomItemFromList list_))

    PickRandomWordResult selectedWord->
      case selectedWord of
        (Nothing, _) -> 
          let 
            _ = Debug.log "No more words" 1
          in
          (model , Cmd.none)
        
        (Just word, list) ->
          let 
            _ = Debug.log "New word choosen" word
          in
          -- update (PlaceSelectedWord word (0,0) (Directions.fromInt 0)) { model | wordList = list}
          ({ model | wordList = list}, Random.generate PlaceSelectedWord (randomStartingValues model.size word))

    PlaceSelectedWord (word, position, direction) -> 
      let 
        _ = Debug.log "Placing " word
        _ = Debug.log "Pos " position
        _ = Debug.log "Dir " direction
      in
        (tryToPlaceWord model (word,position,direction), Cmd.none)


-- Random UTILS

randomString : Int -> Random.Generator String
randomString lenght =
    Random.String.string lenght Random.Char.lowerCaseLatin

randomCellPos: (Int, Int) -> (Int, Int) -> Random.Generator (Int, Int)
randomCellPos (x1, y1) (x2,y2) =
  Random.pair (Random.int x1 x2) (Random.int y1 y2)

randomCellPosFromOrigin : (Int, Int) -> Random.Generator (Int, Int)
randomCellPosFromOrigin (x,y) =
  randomCellPos (0,0) (x,y)

randomItemFromList : (List a) -> Random.Generator ( Maybe a, List a )
randomItemFromList list =
  Random.List.choose list


randomStartingValues: Int -> String -> Random.Generator Job
randomStartingValues size w = 
  Random.map2
    (\point dir -> (w, point, dir))
    (randomCellPos (0,0) (size,size))
    (Directions.random)


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick FillGridWithRandomChars ] [ text "Generate" ]
    , button [ onClick PickRandomWord ] [ text "Filter" ]
    , div [] [ showGrid (WordGrid.toLists model.grid)]
    , ul [] (List.map (\w ->  li [] [text w]) model.wordsToFind)
    -- , div [] [ createRows model.counter ]
    ]

cell : Html Msg
cell = 
  div [ Attribute.class "cell"] [ text " [ ] "]


createColumns : Int -> Html Msg
createColumns times = 
  div
    [ Attribute.class "row"
    ]
    (List.repeat times cell)


createRows : Int -> Html Msg
createRows times = 
  let
    rowList = List.repeat times (createColumns times)
  in
    div
      [ Attribute.class "table"
      ]
      rowList

showGrid: List (List (Maybe Char)) -> Html Msg 
showGrid list = 
  div
    [ Attribute.class "grid"
    ]
    (List.map (\row -> showRow row) list)
  -- case list of 
  --   [] -> 
  --     div
  --     []
  --     [text ""]
  --   x :: [] ->
  --     showRow x
  --   x :: xs -> 
  --     showRow x showGrid 

showRow: List (Maybe Char) -> Html Msg
showRow row = 
  div
    [ Attribute.class "row"
    ]
    (List.map (\a -> div [Attribute.class "cell"][text (showChar a)]) row)

showChar: Maybe Char -> String
showChar posibleChar = 
  case posibleChar of
    Just a -> String.fromChar a
    Nothing -> " "

generateGame: Model -> WordGrid
generateGame model =
  let 
    _ = Debug.log "GENERATING" 1
  
  in
    model.grid

getWordList: List String
getWordList = 
  ["ability", "able", "about", "above", "abroad", "absence", "absent", "absolute", "accept", "accident", "accord", "account", "accuse", "accustom", "ache", "across", "act", "action", "active", "actor", "actress", "actual", "add", "address", "admire", "admission", "admit", "adopt", "adoption", "advance", "advantage", "adventure", "advertise", "advice", "advise", "affair", "afford", "afraid", "after", "afternoon"]


tryToPlaceWord: Model -> Job -> Model
tryToPlaceWord model (word, position, direction) = 
  let
    grid = model.grid
    letters = String.toList word
    nextPosition = position 

  --f = (\l -> WordGrid.setCell grid (getNextPosition nextPosition direction) (Just l))
    --grids = List.map f letters


    positions = List.repeat (String.length word) []
      |> List.indexedMap (\i _ -> getNextPosition nextPosition direction i )
    
    grid_ = tryToPlaceChars letters grid positions

    --grid_ = grid
    -- grids = List.map2 
    --   (\pos letter -> WordGrid.setCell grid pos (Just letter))
    --   poss
    --   letters
    
    --success = List.any Maybe.Extra.isNothing grids
    --grid_ = WordGrid.setCell grid position (Just word)
    _ = Debug.log "grids" grid_
    --_ = Debug.log "success" success
    ---last = List.head(List.reverse(grids))
  in
  case grid_ of
    Nothing -> model
    Just g -> ({model | grid = g,  wordsToFind = word :: model.wordsToFind})
  
  --(model)



tryToPlaceChars: (List Char) -> WordGrid -> (List (Int, Int)) -> Maybe WordGrid
tryToPlaceChars chars grid positions =
  let 
    char = List.head(chars)
    chars_ = Maybe.withDefault [] (List.tail(chars))
    position = List.head(positions)
    position_ = Maybe.withDefault [] (List.tail(positions))
    grid_ = WordGrid.setCell grid (Maybe.withDefault (0,0) position) char
    _ = Debug.log "char" char
    _ = Debug.log "chars_" chars_
    _ = Debug.log "position" position
    _ = Debug.log "position_" position_
    _ = Debug.log "grid_" grid_
  in 
    case (char, position, grid_) of
      (Nothing, _, _) ->
        Just grid
      (_, Nothing, _) ->
        Just grid
      (_, _, Just g) -> tryToPlaceChars chars_ g position_
      (_, _, Nothing) -> Nothing

getNextPosition: (Int, Int) -> Direction -> Int -> (Int, Int)
getNextPosition (x, y) direction s=
  let
    (dx, dy) = Directions.toInt direction
    x_ = x + (dx * s)
    y_ = y + (dy * s)
    _ = Debug.log "            Initial" (x,y)
    _ = Debug.log "            NEW" (x_, y_)
  in 
    (x_, y_)

-- placeLetterAndContinue: (Maybe Char) -> WordGrid -> (Int, Int) -> Direction -> WordGrid
-- placeLetterAndContinue letter grid (x,y) direction = 
--   let 
--     f = \c (x_, y_) dir -> (WordGrid.setCell grid ((x_, y_) dir) c)
--   in 
--   case f letter of
--     Just l -> placeLetterAndContinue 


whileJust : (a -> Maybe a) -> a -> a
whileJust f v =
    case f v of
        Just v_ -> whileJust f v_
        Nothing -> v
