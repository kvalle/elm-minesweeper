module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra
import Time


type alias Model =
    { state : GameState
    , board : Board
    , seconds : Int
    }


type alias Board =
    List Cell


type alias Cell =
    { cellState : CellState
    , cellType : CellType
    }


type GameState
    = NotStarted
    | Playing
    | Lost
    | Won


type CellState
    = Open
    | Closed
    | Flagged


type CellType
    = Mine
    | Safe Int


type Msg
    = OpenCell Int
    | FlagCell Int
    | UnflagCell Int
    | NewGame
    | Tick


columns : Int
columns =
    10


rows : Int
rows =
    5


mines : Int
mines =
    9


countFlags : Board -> Int
countFlags board =
    board
        |> List.filter (.cellState >> (==) Flagged)
        |> List.length


initialBoard : List Cell
initialBoard =
    List.map (Cell Closed) [ Safe 1, Mine, Safe 2, Safe 1, Safe 1, Safe 0, Safe 0, Safe 1, Safe 1, Safe 1, Safe 1, Safe 1, Safe 2, Mine, Safe 1, Safe 0, Safe 0, Safe 1, Mine, Safe 1, Safe 0, Safe 0, Safe 2, Safe 2, Safe 3, Safe 2, Safe 2, Safe 2, Safe 1, Safe 1, Safe 0, Safe 0, Safe 1, Mine, Safe 4, Mine, Mine, Safe 1, Safe 1, Safe 1, Safe 0, Safe 0, Safe 1, Safe 2, Mine, Mine, Safe 3, Safe 1, Safe 1, Mine ]


init : ( Model, Cmd Msg )
init =
    ( { state = NotStarted
      , board = initialBoard
      , seconds = 0
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
        |> Maybe.map (\cell -> cell.cellType == Safe 0)
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
                |> List.filter (.cellType >> (==) Mine)
                |> List.filter (.cellState >> (==) Open)
                |> (not << List.isEmpty)

        allEmptyRevealed =
            model.board
                |> List.filter (.cellType >> (/=) Mine)
                |> List.filter (.cellState >> (==) Closed)
                |> List.isEmpty
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
        NewGame ->
            init

        Tick ->
            ( { model | seconds = model.seconds + 1 }
            , Cmd.none
            )

        OpenCell index ->
            let
                newModel =
                    { model
                        | board = openCell index model.board
                        , state = Playing
                    }
            in
                ( newModel |> updateGameState
                , Cmd.none
                )

        FlagCell index ->
            ( { model | board = flagCell index model.board }, Cmd.none )

        UnflagCell index ->
            ( { model | board = unflagCell index model.board }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ div [ class "game-info" ]
            [ div [ class "game-info--mines" ]
                [ text << toString <| mines - countFlags model.board
                ]
            , button [ class "game-info--state", onClick NewGame ]
                [ text <|
                    case model.state of
                        NotStarted ->
                            ":)"

                        Playing ->
                            ":)"

                        Won ->
                            "B)"

                        Lost ->
                            ":("
                ]
            , div [ class "game-info--time" ]
                [ text << toString <| model.seconds ]
            ]
        , div
            [ class <|
                "board "
                    ++ (case model.state of
                            NotStarted ->
                                "game--playing"

                            Playing ->
                                "game--playing"

                            Won ->
                                "game--finished"

                            Lost ->
                                "game--finished"
                       )
            , style
                [ ( "width", toString (columns * 20) ++ "px" )
                ]
            ]
            (List.indexedMap (viewCell model.state) model.board)
        ]


viewCell : GameState -> Int -> Cell -> Html Msg
viewCell gameState index cell =
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
                , disabled <| List.member gameState [ Won, Lost ]
                ]
                [ text "" ]

        Open ->
            case cell.cellType of
                Mine ->
                    button
                        [ class "cell cell--open cell--mine"
                        , disabled True
                        ]
                        [ text "*" ]

                Safe number ->
                    button
                        [ class <| "cell cell--open cell--free-" ++ toString number
                        , disabled True
                        ]
                        [ if number > 0 then
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
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        NotStarted ->
            Sub.none

        Playing ->
            Time.every Time.second (always Tick)

        Won ->
            Sub.none

        Lost ->
            Sub.none


onRightClick : Msg -> Attribute Msg
onRightClick message =
    onWithOptions
        "contextmenu"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.Decode.succeed message)
