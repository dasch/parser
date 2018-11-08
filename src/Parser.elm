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


succeed : a -> Parser a
succeed val state =
    Ok ( state, val )


fail : String -> Parser a
fail str state =
    Err str


lazy : (() -> Parser a) -> Parser a
lazy f state =
    let
        parser =
            f ()
    in
        parser state


withError : String -> Parser a -> Parser a
withError msg parser state =
    parser state
        |> Result.mapError (\_ -> msg)


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen next parser state =
    parser state
        |> Result.andThen
            (\( newState, val ) -> next val newState)


map : (a -> b) -> Parser a -> Parser b
map f parser =
    parser
        |> andThen (\x -> succeed (f x))


followedBy : Parser a -> Parser (a -> b) -> Parser b
followedBy next =
    andThen
        (\f ->
            next
                |> andThen (\x -> succeed (f x))
        )


ignoring : Parser a -> Parser b -> Parser b
ignoring next =
    andThen
        (\b ->
            next
                |> andThen (\_ -> succeed b)
        )


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser state =
    let
        agg values currState =
            case parser currState of
                Ok ( newState, val ) ->
                    agg (val :: values) newState

                Err err ->
                    ( currState, List.reverse values )
    in
        Ok (agg [] state)


oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    parser
        |> andThen (\val -> map ((::) val) (zeroOrMore parser))


sequence : List (Parser a) -> Parser (List a)
sequence parsers =
    case parsers of
        [] ->
            succeed []

        parser :: rest ->
            parser
                |> andThen (\x -> map (\xs -> x :: xs) (sequence rest))


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
        List.foldl step (Err "") parsers
            |> Result.mapError (\_ -> "expected one of the parsers to match")


peek : Int -> State -> List Char
peek length state =
    List.take length state.remaining


advance : Int -> State -> State
advance length state =
    { state
        | position = state.position + length
        , remaining = List.drop length state.remaining
    }


end : Parser ()
end state =
    if state.remaining == [] then
        Ok ( state, () )
    else
        Err "expected end"


anyChar : Parser Char
anyChar state =
    List.head state.remaining
        |> Maybe.map (\chr -> ( advance 1 state, chr ))
        |> Result.fromMaybe "expected any char"


chomp : Int -> Parser String
chomp n =
    List.repeat n (anyChar)
        |> sequence
        |> map String.fromList


char : Char -> Parser Char
char chr state =
    if peek 1 state == [ chr ] then
        Ok ( advance 1 state, chr )
    else
        Err ("expected char " ++ String.fromChar chr)


string : String -> Parser String
string str =
    String.toList str
        |> List.map char
        |> sequence
        |> map String.fromList


int : Parser Int
int =
    let
        digit : Parser Char
        digit =
            oneOf (List.map char [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ])

        parseInt : List Char -> State -> Result String ( State, Int )
        parseInt digits newState =
            String.fromList digits
                |> String.toInt
                |> Result.fromMaybe "invalid int"
                |> Result.map (\x -> ( newState, x ))
    in
        oneOrMore digit
            |> andThen (\digits -> parseInt digits)
            |> withError "expected int"
