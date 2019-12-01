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
          ( model , Random.generate PlaceSelectedWord (randomStartingValues model.size word))

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
    (randomCellPos (0,0) (size-1,size-1))
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
  ["ability"
  ,"able"
  ,"about"
  ,"above"
  ,"abroad"
  ,"absence"
  ,"absent"
  ,"absolute"
  ,"accept"
  ,"accident"
  ,"accord"
  ,"account"
  ,"accuse"
  ,"accustom"
  ,"ache"
  ,"across"
  ,"act"
  ,"action"
  ,"active"
  ,"actor"
  ,"actress"
  ,"actual"
  ,"add"
  ,"address"
  ,"admire"
  ,"admission"
  ,"admit"
  ,"adopt"
  ,"adoption"
  ,"advance"
  ,"advantage"
  ,"adventure"
  ,"advertise"
  ,"advice"
  ,"advise"
  ,"affair"
  ,"afford"
  ,"afraid"
  ,"after"
  ,"afternoon"
  ,"again"
  ,"against"
  ,"age"
  ,"agency"
  ,"agent"
  ,"ago"
  ,"agree"
  ,"agriculture"
  ,"ahead"
  ,"aim"
  ,"air"
  ,"airplane"
  ,"alike"
  ,"alive"
  ,"all"
  ,"allow"
  ,"allowance"
  ,"almost"
  ,"alone"
  ,"along"
  ,"aloud"
  ,"already"
  ,"also"
  ,"although"
  ,"altogether"
  ,"always"
  ,"ambition"
  ,"ambitious"
  ,"among"
  ,"amongst"
  ,"amount"
  ,"amuse"
  ,"ancient"
  ,"and"
  ,"anger"
  ,"angle"
  ,"angry"
  ,"animal"
  ,"annoy"
  ,"annoyance"
  ,"another"
  ,"answer"
  ,"anxiety"
  ,"anxious"
  ,"any"
  ,"anybody"
  ,"anyhow"
  ,"anyone"
  ,"anything"
  ,"anyway"
  ,"anywhere"
  ,"apart"
  ,"apology"
  ,"appear"
  ,"appearance"
  ,"applaud"
  ,"applause"
  ,"apple"
  ,"application"
  ,"apply"
  ,"appoint"
  ,"approve"
  ,"arch"
  ,"argue"
  ,"arise"
  ,"arm"
  ,"army"
  ,"around"
  ,"arrange"
  ,"arrest"
  ,"arrive"
  ,"arrow"
  ,"art"
  ,"article"
  ,"artificial"
  ,"as"
  ,"ash"
  ,"ashamed"
  ,"aside"
  ,"ask"
  ,"asleep"
  ,"association"
  ,"astonish"
  ,"at"
  ,"attack"
  ,"attempt"
  ,"attend"
  ,"attention"
  ,"attentive"
  ,"attract"
  ,"attraction"
  ,"attractive"
  ,"audience"
  ,"aunt"
  ,"autumn"
  ,"avenue"
  ,"average"
  ,"avoid"
  ,"avoidance"
  ,"awake"
  ,"away"
  ,"awkward"
  ,"axe"
  ,"baby"
  ,"back"
  ,"backward"
  ,"bad"
  ,"bag"
  ,"baggage"
  ,"bake"
  ,"balance"
  ,"ball"
  ,"band"
  ,"bank"
  ,"bar"
  ,"barber"
  ,"bare"
  ,"bargain"
  ,"barrel"
  ,"base"
  ,"basic"
  ,"basin"
  ,"basis"
  ,"basket"
  ,"bath"
  ,"bathe"
  ,"battery"
  ,"battle"
  ,"bay"
  ,"be"
  ,"beak"
  ,"beam"
  ,"bean"
  ,"bear"
  ,"beard"
  ,"beast"
  ,"beat"
  ,"beauty"
  ,"because"
  ,"become"
  ,"bed"
  ,"bedroom"
  ,"before"
  ,"beg"
  ,"begin"
  ,"behave"
  ,"behavior"
  ,"behind"
  ,"being"
  ,"belief"
  ,"believe"
  ,"bell"
  ,"belong"
  ,"below"
  ,"belt"
  ,"bend"
  ,"beneath"
  ,"berry"
  ,"beside"
  ,"besides"
  ,"best"
  ,"better"
  ,"between"
  ,"beyond"
  ,"bicycle"
  ,"big"
  ,"bill"
  ,"bind"
  ,"bird"
  ,"birth"
  ,"bit"
  ,"bite"
  ,"bitter"
  ,"black"
  ,"blade"
  ,"blame"
  ,"bleed"
  ,"bless"
  ,"blind"
  ,"block"
  ,"blood"
  ,"blow"
  ,"blue"
  ,"board"
  ,"boast"
  ,"boat"
  ,"body"
  ,"boil"
  ,"bold"
  ,"bone"
  ,"book"
  ,"border"
  ,"borrow"
  ,"both"
  ,"bottle"
  ,"bottom"
  ,"bound"
  ,"boundary"
  ,"bow"
  ,"bowl"
  ,"box"
  ,"boy"
  ,"brain"
  ,"branch"
  ,"brass"
  ,"brave"
  ,"bravery"
  ,"bread"
  ,"breadth"
  ,"break"
  ,"breakfast"
  ,"breath"
  ,"breathe"
  ,"bribe"
  ,"bribery"
  ,"brick"
  ,"bridge"
  ,"bright"
  ,"brighten"
  ,"bring"
  ,"broad"
  ,"broadcast"
  ,"brother"
  ,"brown"
  ,"brush"
  ,"bucket"
  ,"build"
  ,"bunch"
  ,"bundle"
  ,"burn"
  ,"burst"
  ,"bury"
  ,"bus"
  ,"bush"
  ,"business"
  ,"businesslike"
  ,"businessman"
  ,"busy"
  ,"but"
  ,"butter"
  ,"button"
  ,"buy"
  ,"by"
  ]


tryToPlaceWord: Model -> Job -> Model
tryToPlaceWord model (word, position, direction) = 
  let
    grid = model.grid
    letters = String.toList word
    nextPosition = position 

    grid_ = tryToPlaceChars letters grid position direction

  in
  case grid_ of
    Nothing -> model
    Just g -> (
      {model |
       grid = g
       ,  wordsToFind = word :: model.wordsToFind
       , wordList = List.filter (\w -> w /= word) model.wordList}
      )
  
  --(model)



tryToPlaceChars: (List Char) -> WordGrid -> (Int, Int) -> Direction -> Maybe WordGrid
tryToPlaceChars chars grid position dir=
  let 
    sanityCheck = positionOutOfBounds position grid
    char = List.head(chars)
    chars_ = Maybe.withDefault [] (List.tail(chars))
    position_ = getNextPosition position dir
    grid_ = WordGrid.setCell grid position char
    -- _ = Debug.log "      char" char
    -- _ = Debug.log "      chars_" chars_
    -- _ = Debug.log "      position" position
    -- _ = Debug.log "      position_" position_
  in 
    case (sanityCheck, char,  grid_) of
      (True, _, _) -> Nothing
      (_, Nothing, _) -> Just grid
      (_, _, Just g) -> tryToPlaceChars chars_ g position_ dir
      (_, _, Nothing) -> Nothing

getNextPosition: (Int, Int) -> Direction -> (Int, Int)
getNextPosition (x, y) direction=
  let
    (dx, dy) = Directions.toInt direction
    x_ = x + dx
    y_ = y + dy
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


positionOutOfBounds: (Int, Int) -> WordGrid -> Bool
positionOutOfBounds (x, y) grid=
  let
    size = WordGrid.getSize grid
    -- _ = Debug.log "               x" x
    -- _ = Debug.log "               y" y
    -- _ = Debug.log "               size" size
  in
  (x >= size) || (y >= size) || (x < 0) || (y < 0)