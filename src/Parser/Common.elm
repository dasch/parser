module Parser.Common exposing
    ( int, word
    , alpha, alphaNum, digit, upper, lower
    , space, tab, blank, blanks, newline
    )

{-| Common parsers that can be used in many contexts.


# High Level Parsers

@docs int, word


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


{-| Matches an alphabetic character.
-}
alpha : Parser Char
alpha =
    when Char.isAlpha


{-| Matches an alphanumeric character.
-}
alphaNum : Parser Char
alphaNum =
    when Char.isAlphaNum


{-| Matches a digit, e.g. `8`.
-}
digit : Parser Char
digit =
    when Char.isDigit


{-| Matches an uppercase alphabetic characters, e.g. `A`.
-}
upper : Parser Char
upper =
    when Char.isUpper


{-| Matches a lowercase alphabetic characters, e.g. `z`.
-}
lower : Parser Char
lower =
    when Char.isLower


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
