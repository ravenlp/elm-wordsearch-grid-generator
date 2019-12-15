port module Main exposing (..)

import Browser
import Random
import Random.String
import Random.Char
import Random.List
import Maybe.Extra

import Html exposing (Html, h1, button, div, text, ul, li)
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
  , done: Bool
  }
  

type alias Flags =
  {}

type alias Job =
  ( String, ( Int, Int ), Direction )


-- INIT

init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    size = 12
    fillRatio = 1 -- Bigger the number more words are placed on the grid, more time to generate it
    attempts = round(size * size * fillRatio)
    model_ = 
      { grid = WordGrid.create size Nothing
      , size = size
      , attempts = attempts
      , wordList = []
      , wordsToFind = []
      , done = False
      }
  in
    (model_, fetchWordList)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

port external : String -> Cmd msg

-- UPDATE


type Msg
  = Reset Int Int 
  | GenerateGame
  | FillGridWithRandomChars_ String
  | PickRandomWord
  | PickRandomWordResult (Maybe String, List String)
  | PlaceSelectedWord Job
  | StoreWordList (Result Http.Error (List String))
  | Print



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    Reset size attempts ->
      let 
        model_ =
          { grid = WordGrid.create size Nothing
          , size = size
          , attempts = attempts
          , wordList = []
          , wordsToFind = []
          , done = False
          }
      in 
        (model_, fetchWordList)

    GenerateGame ->
      (model, Cmd.none)
        |> Update.Extra.sequence update ((List.repeat model.attempts PickRandomWord ++ [PickRandomWord]))
        --|> Update.Extra.updateModel (\m -> { m |  grid = createEmptyGrid model.size, wordList = []})

    FillGridWithRandomChars_ randomWord->
      let 
        grid_ = WordGrid.fillEmpty model.grid (String.toList randomWord)
      in
      ({model | grid = grid_}, Cmd.none)
         
    PickRandomWord -> 
      let 
        list_ = List.filter (\s -> (String.length s) <= model.size) model.wordList
      in
      ({ model | wordList = list_ }, Random.generate PickRandomWordResult (randomItemFromList list_))

    PickRandomWordResult selectedWord->
      let 
        continue = model.attempts > 0
        word = Tuple.first selectedWord
      in 
        case (continue, word) of
          (False, _) -> 
            -- Enough attempts 
            ({model | done = True} , completeGrid model)

          (True, Nothing) ->
            -- No more words
            (model , completeGrid model)

          (True, Just w) ->
            ( {model | attempts = model.attempts-1} , Random.generate PlaceSelectedWord (randomStartingValues model.size w))

    PlaceSelectedWord (word, position, direction) ->
      let
        model_ = tryToPlaceWord model (word,position,direction)
      in
      (model_ , Cmd.none)

    StoreWordList result ->
      case result of
        Ok list ->
          ({model | wordList = list}, Cmd.none)

        Err _ ->
          (model, Cmd.none)

    Print -> 
      (model, external "print")

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

createEmptyGrid: Int -> WordGrid
createEmptyGrid size =
  WordGrid.create size Nothing

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
       , wordsToFind = word :: model.wordsToFind
       , wordList = List.filter (\w -> w /= word) model.wordList}
      )
  

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
  in
  (x >= size) || (y >= size) || (x < 0) || (y < 0)

completeGrid: Model -> Cmd Msg
completeGrid model = model.grid
          |> WordGrid.getSize
          |> \x -> x * x
          |> randomString
          |> Random.generate FillGridWithRandomChars_


-- VIEW 

view : Model -> Html Msg
view model =
  let 
    actionButton = if model.done
      then button [ onClick Print ] [ text "Print" ]
      else button [ onClick GenerateGame ] [ text "Generate game" ]
  in
  div [Attribute.class "container"]
    [ div [Attribute.class "actions"] [h1 [] [text "Word search generator"], actionButton]
    , div [] [ showGrid (WordGrid.toLists model.grid)]
    , ul [ Attribute.class "wordList"] (List.map (\w ->  li [] [text w]) model.wordsToFind)
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
