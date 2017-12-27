module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra


type alias Model =
    { state : GameState
    , board : Board
    }


type alias Board =
    List Cell


type alias Cell =
    { cellState : CellState
    , cellType : CellType
    }


type GameState
    = Playing
    | Lost
    | Won


type CellState
    = Open
    | Closed
    | Flagged


type CellType
    = Bomb
    | Free Int


type Msg
    = OpenCell Int
    | FlagCell Int
    | UnflagCell Int


columns : Int
columns =
    10


rows : Int
rows =
    5


init : ( Model, Cmd Msg )
init =
    ( { state = Playing
      , board = List.map (Cell Closed) [ Free 1, Bomb, Free 2, Free 1, Free 1, Free 0, Free 0, Free 1, Free 1, Free 1, Free 1, Free 1, Free 2, Bomb, Free 1, Free 0, Free 0, Free 1, Bomb, Free 1, Free 0, Free 0, Free 2, Free 2, Free 3, Free 2, Free 2, Free 2, Free 1, Free 1, Free 0, Free 0, Free 1, Bomb, Free 4, Bomb, Bomb, Free 1, Free 1, Free 1, Free 0, Free 0, Free 1, Free 2, Bomb, Bomb, Free 3, Free 1, Free 1, Bomb ]
      }
    , Cmd.none
    )


setState : CellState -> Cell -> Cell
setState cellState cell =
    { cell | cellState = cellState }


neighbours : Int -> List Int
neighbours index =
    let
        toIndex ( col, row ) =
            row * columns + col

        col =
            index % columns

        row =
            index // columns

        removeIllegal (( col, row ) as pos) =
            if col >= columns || col < 0 || row > rows || row < 0 then
                Nothing
            else
                Just pos
    in
        [ ( col - 1, row - 1 )
        , ( col - 1, row + 0 )
        , ( col - 1, row + 1 )
        , ( col + 0, row - 1 )
        , ( col + 0, row + 0 )
        , ( col + 0, row + 1 )
        , ( col + 1, row - 1 )
        , ( col + 1, row + 0 )
        , ( col + 1, row + 1 )
        ]
            |> List.filterMap removeIllegal
            |> List.map toIndex


cellEmpty : Int -> List Cell -> Bool
cellEmpty index model =
    model
        |> List.Extra.getAt index
        |> Maybe.map (\cell -> cell.cellType == Free 0)
        |> Maybe.withDefault False


openCell : Int -> Board -> Board
openCell index model =
    let
        getIndicesToOpen index acc =
            if List.member index acc then
                acc
            else if not <| cellEmpty index model then
                index :: acc
            else
                List.foldl getIndicesToOpen
                    (index :: acc)
                    (neighbours index)

        indices =
            getIndicesToOpen index []
    in
        List.Extra.updateIfIndex
            (flip List.member indices)
            (setState Open)
            model


flagCell : Int -> Board -> Board
flagCell index model =
    List.Extra.updateAt index (setState Flagged) model


unflagCell : Int -> Board -> Board
unflagCell index model =
    List.Extra.updateAt index (setState Closed) model


updateGameState : Model -> Model
updateGameState model =
    let
        detonatedMines =
            model.board
                |> List.filter (.cellType >> (==) Bomb)
                |> List.filter (.cellState >> (==) Open)
                |> (not << List.isEmpty)

        allEmptyRevealed =
            model.board
                |> List.filter (.cellType >> (/=) Bomb)
                |> List.filter (.cellState >> (==) Closed)
                |> (not << List.isEmpty)
    in
        if detonatedMines then
            { model | state = Lost }
        else if allEmptyRevealed then
            { model | state = Won }
        else
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenCell index ->
            let
                newModel =
                    { model | board = openCell index model.board }
            in
                ( newModel
                    |> updateGameState
                , Cmd.none
                )

        FlagCell index ->
            ( { model | board = flagCell index model.board }, Cmd.none )

        UnflagCell index ->
            ( { model | board = unflagCell index model.board }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h3 [] [ text "Minesweeper" ]
        , span []
            [ text <|
                case model.state of
                    Playing ->
                        ""

                    Won ->
                        "Game won!"

                    Lost ->
                        "Game lost!"
            ]
        , div
            [ class "board"
            , style
                [ ( "width", toString (columns * 20) ++ "px" )
                ]
            ]
            (List.indexedMap viewCell model.board)
        ]


viewCell : Int -> Cell -> Html Msg
viewCell index cell =
    case cell.cellState of
        Flagged ->
            button
                [ class "cell cell--closed cell--flagged"
                , onRightClick <| UnflagCell index
                ]
                [ text "!" ]

        Closed ->
            button
                [ class "cell cell--closed"
                , onClick <| OpenCell index
                , onRightClick <| FlagCell index
                ]
                [ text "" ]

        Open ->
            button
                [ class "cell cell--open"
                , disabled True
                ]
                [ case cell.cellType of
                    Bomb ->
                        text "*"

                    Free number ->
                        if number > 0 then
                            text <| toString number
                        else
                            text ""
                ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }


onRightClick : Msg -> Attribute Msg
onRightClick message =
    onWithOptions
        "contextmenu"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.Decode.succeed message)
