module Parser.Common
    exposing
        ( int
        , word
        , alpha
        , alphaNum
        , digit
        , upper
        , lower
        , space
        , tab
        , blank
        , blanks
        , newline
        )

import Parser exposing (..)


int : Parser Int
int =
    let
        parseInt : List Char -> State -> Result Error ( State, Int )
        parseInt digits newState =
            String.fromList digits
                |> String.toInt
                |> Result.fromMaybe (Error "invalid int" newState.position)
                |> Result.map (\x -> ( newState, x ))
    in
        oneOrMore digit
            |> andThen (\digits -> parseInt digits)
            |> withError "expected int"


word : Parser String
word =
    let
        wordChar =
            oneOf [ alphaNum, char '_' ]
    in
        oneOrMore wordChar
            |> map String.fromList
            |> withError "expected word"


alpha : Parser Char
alpha =
    when Char.isAlpha


alphaNum : Parser Char
alphaNum =
    when Char.isAlphaNum


digit : Parser Char
digit =
    when Char.isDigit


upper : Parser Char
upper =
    when Char.isUpper


lower : Parser Char
lower =
    when Char.isLower


space : Parser Char
space =
    char ' '


tab : Parser Char
tab =
    char '\t'


blank : Parser Char
blank =
    oneOf [ space, tab ]


blanks : Parser String
blanks =
    map String.fromList (zeroOrMore blank)


newline : Parser Char
newline =
    char '\n'
