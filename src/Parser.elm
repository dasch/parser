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


orElse : Parser a -> Parser a -> Parser a
orElse fallback parser state =
    case parser state of
        Err _ ->
            fallback state

        Ok ( newState, x ) ->
            Ok ( newState, x )


map : (a -> b) -> Parser a -> Parser b
map f parser =
    parser
        |> andThen (\x -> succeed (f x))


into : (a -> b) -> Parser (a -> b)
into =
    succeed


grab : Parser a -> Parser (a -> b) -> Parser b
grab next =
    andThen
        (\f ->
            next
                |> andThen (\x -> succeed (f x))
        )


ignore : Parser a -> Parser b -> Parser b
ignore next =
    andThen
        (\b ->
            next
                |> andThen (\_ -> succeed b)
        )


maybe : Parser a -> Parser (Maybe a)
maybe parser =
    parser
        |> map Just
        |> orElse (succeed Nothing)


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
oneOf parsers =
    List.foldl orElse (fail "") parsers
        |> withError "expected one of the parsers to match"


peek : Int -> State -> List Char
peek length state =
    List.take length state.remaining


advance : Int -> State -> State
advance length state =
    { state
        | position = state.position + length
        , remaining = List.drop length state.remaining
    }


until : Parser a -> Parser b -> Parser (List b)
until stop parser state =
    let
        follow =
            parser
                |> andThen
                    (\x ->
                        until stop parser
                            |> map (\xs -> x :: xs)
                    )
    in
        case stop state of
            Ok _ ->
                Ok ( state, [] )

            Err _ ->
                follow state


between : Parser a -> Parser b -> Parser c -> Parser (List c)
between open close inner =
    succeed identity
        |> ignore open
        |> grab (until close inner)
        |> ignore close


separatedBy : Parser s -> Parser a -> Parser (List a)
separatedBy separator parser =
    let
        empty =
            succeed []

        oneElement =
            parser
                |> map (List.singleton)

        multipleElements =
            succeed (::)
                |> grab parser
                |> ignore separator
                |> grab (lazy (\_ -> separatedBy separator parser))
    in
        oneOf [ multipleElements, oneElement, empty ]


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


anyCharExcept : Char -> Parser Char
anyCharExcept chr =
    when (\c -> c /= chr)


chomp : Int -> Parser String
chomp n =
    List.repeat n (anyChar)
        |> sequence
        |> map String.fromList


when : (Char -> Bool) -> Parser Char
when predicate state =
    case state.remaining of
        chr :: _ ->
            if predicate chr then
                Ok ( advance 1 state, chr )
            else
                Err ("char " ++ String.fromChar chr ++ " failed predicate")

        _ ->
            Err "end of input"


except : Parser Char -> Parser Char
except parser state =
    case parser state of
        Ok _ ->
            Err "expected to not match"

        Err _ ->
            anyChar state


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
