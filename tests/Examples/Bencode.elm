module Examples.Bencode exposing (parse, Value(..))

import Parser exposing (..)
import Dict exposing (Dict)


type Value
    = BString String
    | BInt Int
    | BList (List Value)
    | BDict (Dict String Value)


parse : String -> Result String Value
parse input =
    Parser.parse input value


value : Parser Value
value =
    oneOf [ int, list ]


int : Parser Value
int =
    succeed BInt
        |> ignoring (char 'i')
        |> followedBy Parser.int
        |> ignoring e


list : Parser Value
list =
    succeed BList
        |> ignoring (char 'l')
        |> followedBy (zeroOrMore (lazy (\_ -> value)))
        |> ignoring e


e : Parser Char
e =
    char 'e'
