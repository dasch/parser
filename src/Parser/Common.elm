module Parser.Common exposing (..)

import Parser exposing (..)


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
