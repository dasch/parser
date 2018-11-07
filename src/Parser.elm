module Parser exposing (..)


type alias Parser a =
    State -> Result String ( State, a )


type alias State =
    { input : String
    , remaining : List Char
    , position : Int
    }


init : String -> State
init input =
    { input = input
    , remaining = String.toList input
    , position = 0
    }


parse : String -> Parser a -> Result String a
parse input parser =
    case parser (init input) of
        Ok ( state, value ) ->
            Ok value

        Err err ->
            Err err


char : Char -> Parser ()
char chr state =
    if peek 1 state == [ chr ] then
        Ok ( advance 1 state, () )
    else
        Err "expected char"


anyChar : Parser Char
anyChar state =
    List.head state.remaining
        |> Maybe.map (\chr -> ( advance 1 state, chr ))
        |> Result.fromMaybe "expected any char"


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen next parser state =
    case parser state of
        Ok ( newState, val ) ->
            next val newState

        Err err ->
            Err err


peek : Int -> State -> List Char
peek length state =
    List.take length state.remaining


advance : Int -> State -> State
advance length state =
    { state
        | position = state.position + length
        , remaining = List.drop length state.remaining
    }
