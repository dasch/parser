module Parser.Common exposing
    ( int, float, word
    , iso8601
    , alpha, alphaNum, digit, upper, lower
    , space, tab, blank, blanks, newline
    )

{-| Common parsers that can be used in many contexts.


# High Level Parsers

@docs int, float, word


# Date & Time

@docs iso8601


# Single-Character Parsers

@docs alpha, alphaNum, digit, upper, lower


# Whitespace

@docs space, tab, blank, blanks, newline

-}

import Parser exposing (..)


{-| Matches an integer.

    parse "42" int -- Ok 42

-}
int : Parser Int
int =
    let
        parseInt : String -> Parser Int
        parseInt digits =
            case String.toInt digits of
                Just x ->
                    succeed x

                Nothing ->
                    fail "invalid int"
    in
    stringWith (oneOrMore digit)
        |> andThen parseInt
        |> withError "expected int"


{-| Matches a float.

    parse "4.2" float -- Ok 4.2

-}
float : Parser Float
float =
    let
        parseFloat : String -> Parser Float
        parseFloat digits =
            case String.toFloat digits of
                Just x ->
                    succeed x

                Nothing ->
                    fail "invalid float"
    in
    maybe (char '-')
        |> followedBy (oneOrMore digit)
        |> followedBy (char '.')
        |> followedBy (oneOrMore digit)
        |> matchedString
        |> andThen parseFloat
        |> withError "expected float"


{-| Matches a "word", comprised of alphanumeric characters and `_`.

    parse "hello world" word -- Ok "hello"

-}
word : Parser String
word =
    let
        wordChar =
            oneOf [ alphaNum, char '_' ]
    in
    oneOrMore wordChar
        |> map String.fromList
        |> withError "expected word"


{-| Matches valid [ISO8601](https://en.wikipedia.org/wiki/ISO_8601)
datetimes.
-}
iso8601 : Parser String
iso8601 =
    let
        date =
            year
                |> followedBy (char '-')
                |> followedBy month
                |> followedBy (char '-')
                |> followedBy day

        time =
            char 'T'
                |> followedBy hour
                |> followedBy (char ':')
                |> followedBy minute
                |> followedBy (char ':')
                |> followedBy second
                |> followedBy (maybe fraction)
                |> followedBy (maybe zone)

        zone =
            oneOf [ dropValue (char 'Z'), offset ]

        offset =
            oneOf [ char '-', char '+' ]
                |> followedBy hour
                |> followedBy (char ':')
                |> followedBy minute
                |> dropValue

        year =
            repeat 4 digit

        month =
            repeat 2 digit

        day =
            repeat 2 digit

        hour =
            repeat 2 digit

        minute =
            repeat 2 digit

        second =
            repeat 2 digit

        fraction =
            char '.'
                |> followedBy (upto 9 digit)
    in
    date
        |> followedBy (maybe time)
        |> matchedString


{-| Matches an alphabetic character.
-}
alpha : Parser Char
alpha =
    when Char.isAlpha
        |> withError "expected alphabetic char"


{-| Matches an alphanumeric character.
-}
alphaNum : Parser Char
alphaNum =
    when Char.isAlphaNum
        |> withError "expected alphanumeric char"


{-| Matches a digit, e.g. `8`.
-}
digit : Parser Char
digit =
    when Char.isDigit
        |> withError "expected digit"


{-| Matches an uppercase alphabetic characters, e.g. `A`.
-}
upper : Parser Char
upper =
    when Char.isUpper
        |> withError "expected uppercase char"


{-| Matches a lowercase alphabetic characters, e.g. `z`.
-}
lower : Parser Char
lower =
    when Char.isLower
        |> withError "expected lowercase char"


{-| Matches the space character, \`\`.
-}
space : Parser Char
space =
    char ' '


{-| Matches the tab character, `\t`.
-}
tab : Parser Char
tab =
    char '\t'


{-| Matches either a space or a tab.
-}
blank : Parser Char
blank =
    oneOf [ space, tab ]


{-| Matches zero or more [`blank`](#blank) characters.
-}
blanks : Parser String
blanks =
    map String.fromList (zeroOrMore blank)


{-| Matches a newline character.
-}
newline : Parser Char
newline =
    char '\n'


dropValue : Parser a -> Parser ()
dropValue parser =
    parser
        |> followedBy (succeed ())


upto : Int -> Parser a -> Parser (List a)
upto n parser =
    if n == 0 then
        succeed []

    else if n > 0 then
        parser
            |> followedBy (upto (n - 1) parser)
            |> orElse (succeed [])

    else
        fail "n must be at least 0"
