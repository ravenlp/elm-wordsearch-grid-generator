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


import Html exposing (Html, button, div, text)
import Html.Attributes as Attribute
import Html.Events exposing (onClick)


import WordGrid exposing (WordGrid)



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
  }
  

type alias Flags =
  {}


init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    test = WordGrid.create 5 ' '
    model_ = 
      { grid = test
      , size = 5
      , counter= 10
      , wordList= getWordList}
    
    _ = Debug.log "WordGrid" (WordGrid.toList test)
    _ = Debug.log "WordGrid" (WordGrid.getSize test)
    _ = Debug.log "WordGrid" (WordGrid.toLists test)
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
          ({ model | wordList = list}, Cmd.none)


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
  

randomize: Random.Generator Int
randomize = 
  Random.int 1 10


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick FillGridWithRandomChars ] [ text "Generate" ]
    , button [ onClick PickRandomWord ] [ text "Filter" ]
    , div [] [ showGrid (WordGrid.toLists model.grid)]
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

showGrid: List (List Char) -> Html Msg 
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

showRow: List Char -> Html Msg
showRow row = 
  div
    [ Attribute.class "row"
    ]
    (List.map (\a -> div [Attribute.class "cell"][text (String.fromChar a)]) row)

generateGame: Model -> WordGrid
generateGame model =
  let 
    _ = Debug.log "GENERATING" 1
  
  in
    model.grid

getWordList: List String
getWordList = 
  ["ability", "able", "about", "above", "abroad", "absence", "absent", "absolute", "accept", "accident", "accord", "account", "accuse", "accustom", "ache", "across", "act", "action", "active", "actor", "actress", "actual", "add", "address", "admire", "admission", "admit", "adopt", "adoption", "advance", "advantage", "adventure", "advertise", "advice", "advise", "affair", "afford", "afraid", "after", "afternoon"]