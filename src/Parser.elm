module Parser
    exposing
        ( Parser
        , Error
        , parse
        , succeed
        , fail
        , lazy
        , withError
        , andThen
        , orElse
        , followedBy
        , map
        , map2
        , into
        , grab
        , ignore
        , maybe
        , zeroOrMore
        , oneOrMore
        , sequence
        , repeat
        , oneOf
        , until
        , separatedBy
        , end
        , anyChar
        , when
        , stringWith
        , matchedString
        , chomp
        , except
        , char
        , string
        )

{-| Easy to use text parsing.

# Definitions
@docs Parser, Error

# Core
@docs parse, succeed, fail, lazy

# Matching Specific Text
@docs char, string

# Matching with Patterns
@docs anyChar, when, except, end, chomp

# Matching Multiple Different Patterns
@docs oneOf

# Matching Sequences
@docs maybe, zeroOrMore, oneOrMore, sequence, repeat, until

# Chaining Parsers
@docs andThen, orElse, followedBy

# Pipelines
@docs into, grab, ignore

# Transforming Parsed Values
@docs map, map2, withError, stringWith, matchedString

# High Level Parsers
@docs separatedBy
-}


{-| A `Parser a` is an instruction for how to take some input text and turn it
into an `a` value as an updated input text that has had some prefix removed.

If a parser fails to turn the input into a value, it fails with `Error`.
-}
type Parser a
    = Parser (State -> Result Error ( State, a ))


{-| The state of a parsing process.
-}
type State
    = State
        { input : String
        , remaining : List Char
        , position : Int
        }


{-| Describes an error during parsing, i.e. what caused a parser to fail, and at
what position into the input text it failed.
-}
type alias Error =
    { message : String
    , position : Int
    }


init : String -> State
init input =
    State
        { input = input
        , remaining = String.toList input
        , position = 0
        }


run : Parser a -> State -> Result Error ( State, a )
run (Parser parser) state =
    parser state


{-| Parse an input string using a specific parser, returning a result containing
either the parsed value or an error.

    parse "xyz" (char 'x') -- Ok 'x'
    parse "xyz" (char 'w') -- Err { message = "expected char w", position = 0 }
-}
parse : String -> Parser a -> Result Error a
parse input parser =
    run parser (init input)
        |> Result.map (\( _, value ) -> value)


{-| A parser that always succeeds with a specified value without reading any input.

    parse "xyz" (succeed 42) -- Ok 42
-}
succeed : a -> Parser a
succeed val =
    Parser <|
        \state ->
            Ok ( state, val )


{-| A parser that always fails with a specified error message without reading any
input.

    parse "xyz" (fail "nope") -- Err { message = "nope", position = 0 }
-}
fail : String -> Parser a
fail str =
    Parser <|
        \(State state) ->
            Err { message = str, position = state.position }


{-| In order to support self-referential parsers, you need to introduce lazy
evaluation.

    type Tree = Leaf | Node Tree Tree

    tree : Parser Tree
    tree =
        oneOf [ leaf, node ]

    leaf : Parser Tree
    leaf =
        map (always Leaf) (char 'x')

    node : Parser Tree
    node =
        into Node
            |> ignore (char '@')
            |> grab (lazy (\_ -> tree))
            |> grab (lazy (\_ -> tree))

    parse "x" tree -- Ok Leaf
    parse "@x@xx" tree -- Ok (Node Leaf (Node Leaf Leaf))

Without `lazy`, this example would fail due to a circular reference.
-}
lazy : (() -> Parser a) -> Parser a
lazy f =
    Parser <|
        \state ->
            let
                (Parser parser) =
                    f ()
            in
                parser state


{-| Use the specified error message when the parser fails.

    string "</div>"
        |> withError "expected closing tag"
-}
withError : String -> Parser a -> Parser a
withError msg parser =
    Parser <|
        \state ->
            run parser state
                |> Result.mapError (\err -> { err | message = msg })


{-| Create a parser that depends on the a previous result.

For example, you can support two different versions of a format if there's
a version number included:

    spec : Parser Spec
    spec =
        let
            specByVersion version =
                case version of
                    1 -> v1 -- assume v1 is a Parser Spec
                    2 -> v2 -- assume v2 is a Parser Spec
                    x -> fail ("unknown spec version " ++ String.fromInt x)
        in
            string "version="
                |> andThen specByVersion
-}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen next parser =
    Parser <|
        \state ->
            run parser state
                |> Result.andThen
                    (\( newState, val ) -> run (next val) newState)


{-| Create a parser that depends on a previous parser succeeding. Unlike
[`andThen`](#andThen), this does not preserve the value of the first parser,
so it's only useful when you want to discard that value.

    atMention : Parser String
    atMention =
        char '@'
            |> followedBy username

-}
followedBy : Parser a -> Parser b -> Parser a
followedBy kept ignored =
    ignored
        |> andThen (\_ -> kept)


{-| Create a fallback for when a parser fails.
-}
orElse : Parser a -> Parser a -> Parser a
orElse fallback parser =
    Parser <|
        \state ->
            case run parser state of
                Err _ ->
                    run fallback state

                Ok ( newState, x ) ->
                    Ok ( newState, x )


{-| Map the value of a parser.

    map (\x -> x * x) int
-}
map : (a -> b) -> Parser a -> Parser b
map f parser =
    parser
        |> andThen (\x -> succeed (f x))


{-| Matches two parsers and combines the result.

    map2 (\x y -> (x, y)) anyChar anyChar
        |> parse "xy" -- Ok ('x', 'y')
-}
map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f p1 p2 =
    p1
        |> andThen
            (\x ->
                p2
                    |> andThen (\y -> succeed (f x y))
            )


{-| Start a parser pipeline that feeds values into a function.

Typically used to build up complex values.

    type Operation = Binary Int Char Int

    operation : Parser Operation
    operation =
        into Operation
            |> grab int
            |> ignore blanks
            |> grab (oneOf [ char '+', char '-', char '*' ])
            |> ignore blanks
            |> grab int

    parse "42 * 13" operation -- Binary 42 '*' 13

Here we feed three values into `Operation` while ignoring blank characters between
the values.
-}
into : (a -> b) -> Parser (a -> b)
into =
    succeed


{-| Grabs a value and feeds it into a function in a pipeline.

See [`into`](#into).
-}
grab : Parser a -> Parser (a -> b) -> Parser b
grab next =
    andThen
        (\f ->
            next
                |> andThen (\x -> succeed (f x))
        )


{-| Ignores a matched value, preserving the previous value in a pipeline.

See [`into`](#into).
-}
ignore : Parser a -> Parser b -> Parser b
ignore next =
    andThen
        (\b ->
            next
                |> andThen (\_ -> succeed b)
        )


{-| Maybe match a value. If the parser succeeds with `x`, we'll succeed with
`Just x`. If if fails, we'll succeed with `Nothing`.

    parse "42" (maybe int) -- Just 42
    parse "hello" (maybe int) -- Nothing
-}
maybe : Parser a -> Parser (Maybe a)
maybe parser =
    parser
        |> map Just
        |> orElse (succeed Nothing)


{-| Matches zero or more successive occurrences of a value. Succeeds with
an empty list if there are no occurrences.

    parse "xxy" (zeroOrMore (char 'x')) -- Ok [ 'x', 'x' ]
    parse "yyy" (zeroOrMore (char 'x')) -- Ok []
-}
zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
    oneOf
        [ map2 (::) parser (lazy (\_ -> zeroOrMore parser))
        , succeed []
        ]


{-| Matches one or more successive occurrences of a value. Fails if
there are no occurrences.

    parse "xxy" (oneOrMore (char 'x')) -- Ok [ 'x', 'x' ]
    parse "yyy" (oneOrMore (char 'x')) -- Err { message = "expected char `x`", position = 0 }
-}
oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
    map2 (::) parser (zeroOrMore parser)


{-| Matches a sequence of parsers in turn, succeeding with a list of
their values if they *all* succeed.

    parse "helloworld" (sequence [ string "hello", string "world" ]) -- Ok [ "hello", "world" ]
-}
sequence : List (Parser a) -> Parser (List a)
sequence parsers =
    case parsers of
        [] ->
            succeed []

        parser :: rest ->
            map2 (::) parser (sequence rest)


{-| Matches a specific number of occurrences of a parser, succeeding with a list
of values.

    parse "xxxx" (repeat 3 (char 'x')) -- Ok [ 'x', 'x', 'x' ]
-}
repeat : Int -> Parser a -> Parser (List a)
repeat n parser =
    sequence (List.repeat n parser)


{-| Matches one of a list of parsers.

    parse "y" (oneOf [ char 'x', char 'y' ]) -- Ok 'y'
-}
oneOf : List (Parser a) -> Parser a
oneOf parsers =
    List.foldl orElse (fail "") parsers
        |> withError "expected one of the parsers to match"


peek : Int -> State -> List Char
peek length (State state) =
    List.take length state.remaining


advance : Int -> State -> State
advance length (State state) =
    State
        { state
            | position = state.position + length
            , remaining = List.drop length state.remaining
        }


{-| Matches zero or more values until a "stop" parser matches.

    char '['
        |> until (char ']') anyChar
        |> parse "[abc]" -- Ok [ 'a', 'b', 'c' ]
-}
until : Parser a -> Parser b -> Parser (List b)
until stop parser =
    Parser <|
        \state ->
            let
                follow =
                    parser
                        |> andThen
                            (\x ->
                                until stop parser
                                    |> map (\xs -> x :: xs)
                            )
            in
                case run stop state of
                    Ok _ ->
                        Ok ( state, [] )

                    Err _ ->
                        run follow state


{-| Matches zero or more values separated by a specified parser.

    separatedBy (char ',') int
        |> parse "42,13,99" -- Ok [ 42, 13, 99 ]
-}
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


{-| Matches the end of the input.

    char 'x'
        |> followedBy end
        |> parse "x" -- Ok ()
-}
end : Parser ()
end =
    Parser <|
        \(State state) ->
            if state.remaining == [] then
                Ok ( State state, () )
            else
                Err { message = "expected end", position = state.position }


{-| Matches any character.
-}
anyChar : Parser Char
anyChar =
    Parser <|
        \(State state) ->
            List.head state.remaining
                |> Maybe.map (\chr -> ( advance 1 (State state), chr ))
                |> Result.fromMaybe { message = "expected any char", position = state.position }


{-| Matches a character if some predicate holds.

    parse "123" (when Char.isDigit) -- Ok '1'
-}
when : (Char -> Bool) -> Parser Char
when predicate =
    anyChar
        |> andThen
            (\c ->
                if predicate c then
                    succeed c
                else
                    fail "failed predicate"
            )


{-| Matches a character if the specified parser *fails*.

    parse "xyz" (except (char 'a')) -- Ok 'x'
    parse "xyz" (except (char 'x')) -- Err { message = "expected to not match", ... }
-}
except : Parser Char -> Parser Char
except parser =
    Parser <|
        \state ->
            case run parser state of
                Ok _ ->
                    run (fail "expected to not match") state

                Err _ ->
                    run anyChar state


{-| Turns a parser that returns a list of characters into a parser that
returns a String.

    parse "xyz" (stringWith (sequence [ char 'x', anyChar, char 'z' ])) -- Ok "xyz"
-}
stringWith : Parser (List Char) -> Parser String
stringWith =
    map String.fromList


{-| Maps a parser to include all the matched input as a String.

    matchedString (sequence [ word, string "@", word ])
        |> parse "hello@world!" -- Ok "hello@world"
-}
matchedString : Parser a -> Parser String
matchedString parser =
    Parser <|
        \(State state) ->
            case run parser (State state) of
                Ok ( State newState, _ ) ->
                    let
                        str =
                            String.slice state.position newState.position state.input
                    in
                        Ok ( State newState, str )

                Err err ->
                    Err err


{-| A parser that simply reads a specific number of characters from the
input.

    parse "xyz" (chomp 2) -- Ok "xy"
-}
chomp : Int -> Parser String
chomp n =
    List.repeat n (anyChar)
        |> sequence
        |> map String.fromList


{-| Matches a specific character.

    parse "hello" (char 'h') -- Ok 'h'
-}
char : Char -> Parser Char
char chr =
    Parser <|
        \state ->
            if peek 1 state == [ chr ] then
                Ok ( advance 1 state, chr )
            else
                run (fail ("expected char " ++ String.fromChar chr)) state


{-| Matches a specific string.

    parse "hello world" (string "hello") -- Ok "hello"
-}
string : String -> Parser String
string str =
    String.toList str
        |> List.map char
        |> sequence
        |> map String.fromList
