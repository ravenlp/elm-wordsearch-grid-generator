module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Random

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
  { content: WordGrid String
  , counter: Int
  }

type alias Flags =
  {}


init : Flags -> ( Model, Cmd Msg )
init flags =
  let
    test = WordGrid.fromList ["A","A","B","A","B","C","A","C","A"] 3
    model2 = { content= test, counter= 4}
    
    _ = Debug.log "Grid" (WordGrid.toList test)
    _ = Debug.log "Grid" (WordGrid.getSize test)
    _ = Debug.log "Grid" (WordGrid.toLists test)
    _ = Debug.log "Grid" (WordGrid.getRow test 1)
  in
    (model2, Cmd.none)
  


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- Utils

randomize: Random.Generator Int
randomize = 
  Random.int 1 10

-- UPDATE


type Msg
  = Increment
  | Decrement
  | NewValue Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NewValue number -> 
      let 
        _ = Debug.log "Number" number
      in
        (model , Cmd.none)
    Increment ->
      ({ model | counter = model.counter + 1 }, Random.generate NewValue randomize)
    Decrement ->
      ({ model | counter = model.counter - 1 }, Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (String.fromInt model.counter) ]
    , button [ onClick Increment ] [ text "+" ]
    , div [] [ showGrid (WordGrid.toLists model.content)]
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

showGrid: List (List String) -> Html Msg 
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

showRow: List String -> Html Msg
showRow row = 
  div
    [ Attribute.class "row"
    ]
    (List.map (\a -> div [Attribute.class "cell"][text a]) row)