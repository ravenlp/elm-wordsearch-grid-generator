module Main exposing (..)

import Browser
import Random
import Random.String
import Random.Char
import Random.List
import Maybe.Extra

import Html exposing (Html, button, div, text, ul, li)
import Html.Attributes as Attribute
import Html.Events exposing (onClick)

import Task

import Update.Extra

import Http
import Json.Decode


-- Custom types
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
  , attempts: Int
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
      , attempts= 100
      , wordList= []
      , wordsToFind = []
      }
    
    --_ = Debug.log "WordGrid getRow" (WordGrid.getRow test 0)
    --_ = Debug.log "WordGrid toList" (WordGrid.toList test)
    --_ = Debug.log "WordGrid size" (WordGrid.getSize test)
    --_ = Debug.log "WordGrid toLists" (WordGrid.toLists test)
    --_ = Debug.log "WordGrid" (WordGrid.getRow test 1)
  in
    (model_, fetchWordList)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- UPDATE


type Msg
  = GenerateGame
  | FillGridWithRandomChars 
  | FillGridWithRandomChars_ String
  | PickRandomWord
  | PickRandomWordResult (Maybe String, List String)
  | PlaceSelectedWord Job
  | StoreWordList (Result Http.Error (List String))



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    GenerateGame ->
      (model, Cmd.none)
        |> Update.Extra.sequence update ((List.repeat 100 PickRandomWord ++ [PickRandomWord]))
        --|> Update.Extra.andThen update FillGridWithRandomChars
          --|> Update.Extras.addCmd (Task.succeed PickRandomWord |> Task.perform (\a -> a))

    FillGridWithRandomChars_ randomWord->
      let 
        _ = Debug.log "GetWordList" randomWord
        --grid_ = WordGrid.fromList (String.toList randomWord) (WordGrid.getSize model.grid)
        grid_ = WordGrid.fillEmpty model.grid (String.toList randomWord)
      in
      ({model | grid = grid_}, Cmd.none)

    FillGridWithRandomChars ->
      ( model,
        model.grid
          |> WordGrid.getSize
          |> \x -> x * x
          |> randomString
          |> Random.generate FillGridWithRandomChars_ )
         

    PickRandomWord -> 
      let 
        list_ = List.filter (\s -> (String.length s) <= model.size) model.wordList
      in
      ({ model | wordList = list_ }, Random.generate PickRandomWordResult (randomItemFromList list_))

    PickRandomWordResult selectedWord->
      case selectedWord of
        (Nothing, _) -> 
          -- No more words
          (model , Cmd.none)
        
        (Just word, list) ->
          ( model , Random.generate PlaceSelectedWord (randomStartingValues model.size word))

    PlaceSelectedWord (word, position, direction) -> 
      (tryToPlaceWord model (word,position,direction), Cmd.none)

    StoreWordList result ->
      case result of
        Ok list ->
          ({model | wordList = list}, Cmd.none)

        Err _ ->
          (model, Cmd.none)

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
    (randomCellPos (0,0) (size-1,size-1))
    (Directions.random)


-- HTTP

fetchWordList : Cmd Msg
fetchWordList =
  Http.get
    { url = "http://localhost:8000/wordlist.json"
    , expect = Http.expectJson StoreWordList wordDecoder
    }

wordDecoder: Json.Decode.Decoder (List String)
wordDecoder =
  Json.Decode.list Json.Decode.string

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick GenerateGame ] [ text "Generate" ]
    , button [ onClick PickRandomWord ] [ text "Add single word" ]
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

placeWords: Int -> List (Task.Task x Msg)
placeWords n = 
  List.repeat n (Task.succeed PickRandomWord)

tryToPlaceWord: Model -> Job -> Model
tryToPlaceWord model (word, position, direction) = 
  let
    grid = model.grid
    letters = String.toList word
    grid_ = tryPossibleDirections letters grid position direction 7

  in
  case grid_ of
    Nothing -> 
      model
    Just g -> (
      {model |
       grid = g
       ,  wordsToFind = word :: model.wordsToFind
       , wordList = List.filter (\w -> w /= word) model.wordList}
      )
  
  --(model)

tryPossibleDirections: (List Char) -> WordGrid -> (Int, Int) -> Direction -> Int -> Maybe WordGrid
tryPossibleDirections letters grid position direction n = 
  let
    grid_ = tryToPlaceChars letters grid position direction
    continue = n > 0 
  in
    case (grid_, continue) of
      (Nothing, True) -> 
        -- Unable to fit word, trying the next direction
        tryPossibleDirections letters grid position (Directions.next direction) (n-1)

      (Nothing, False) -> 
        -- Tried all directions, word just wont fit
        Nothing

      (Just g, _) ->
        -- Word placed successfully 
        Just g
      


tryToPlaceChars: (List Char) -> WordGrid -> (Int, Int) -> Direction -> Maybe WordGrid
tryToPlaceChars chars grid position dir =
  let 
    sanityCheck = positionOutOfBounds position grid
    char = List.head(chars)
    chars_ = Maybe.withDefault [] (List.tail(chars))
    position_ = getNextPosition position dir
    grid_ = WordGrid.setCell grid position char
  in 
    case (sanityCheck, char,  grid_) of
      (True, _, _) ->
        -- Position is out of the grid, aborting
        Nothing

      (_, Nothing, _) ->
        -- No more chars left to place, success
        Just grid

      (_, _, Just g) ->
        -- Already placed n char(s), try to place n+1
        tryToPlaceChars chars_ g position_ dir

      (_, _, Nothing) ->
        -- Cell was taken with a different letter
        Nothing

getNextPosition: (Int, Int) -> Direction -> (Int, Int)
getNextPosition (x, y) direction=
  let
    (dx, dy) = Directions.toInt direction
    x_ = x + dx
    y_ = y + dy
  in 
    (x_, y_)

positionOutOfBounds: (Int, Int) -> WordGrid -> Bool
positionOutOfBounds (x, y) grid=
  let
    size = WordGrid.getSize grid
    -- _ = Debug.log "               x" x
    -- _ = Debug.log "               y" y
    -- _ = Debug.log "               size" size
  in
  (x >= size) || (y >= size) || (x < 0) || (y < 0)