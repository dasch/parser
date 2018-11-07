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


oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    parser
        |> andThen
            (\val ->
                zeroOrMore parser
                    |> andThen (\rest -> succeed (val :: rest))
            )


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser state =
    let
        agg values currState =
            if isAtEnd currState then
                ( currState, [] )
            else
                case parser currState of
                    Ok ( newState, val ) ->
                        agg (val :: values) newState

                    Err err ->
                        ( currState, values )
    in
        Ok (agg [] state)


succeed : a -> Parser a
succeed val state =
    Ok ( state, val )


fail : String -> Parser a
fail str state =
    Err str


isAtEnd : State -> Bool
isAtEnd state =
    List.isEmpty state.remaining


char : Char -> Parser ()
char chr state =
    if peek 1 state == [ chr ] then
        Ok ( advance 1 state, () )
    else
        Err "expected char"


string : String -> Parser ()
string str state =
    let
        step : Char -> Result String ( State, () ) -> Result String ( State, () )
        step chr tmp =
            Result.andThen (\( newState, () ) -> char chr newState) tmp
    in
        List.foldl step (succeed () state) (String.toList str)


anyChar : Parser Char
anyChar state =
    List.head state.remaining
        |> Maybe.map (\chr -> ( advance 1 state, chr ))
        |> Result.fromMaybe "expected any char"


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen next parser state =
    parser state
        |> Result.andThen
            (\( newState, val ) -> next val newState)


oneOf : List (Parser a) -> Parser a
oneOf parsers state =
    let
        step : Parser a -> Result String ( State, a ) -> Result String ( State, a )
        step parser tmp =
            case tmp of
                Ok ( newState, val ) ->
                    Ok ( newState, val )

                Err _ ->
                    parser state
    in
        List.foldl step (fail "expected one of the parsers to match" state) parsers


peek : Int -> State -> List Char
peek length state =
    List.take length state.remaining


advance : Int -> State -> State
advance length state =
    { state
        | position = state.position + length
        , remaining = List.drop length state.remaining
    }
