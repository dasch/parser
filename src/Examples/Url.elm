module Examples.Url exposing (Url, parse)

import Parser exposing (..)
import Parser.Common exposing (..)


type alias Url =
    { protocol : String
    , host : String
    , port_ : Maybe Int
    , path : String
    , query : Maybe String
    , fragment : Maybe String
    }


parse : String -> Result Error Url
parse input =
    Parser.parse input url


url : Parser Url
url =
    into Url
        |> grab protocol
        |> ignore (string "://")
        |> grab host
        |> grab (maybe port_)
        |> grab path
        |> grab (maybe query)
        |> grab (maybe fragment)


protocol : Parser String
protocol =
    stringWith (oneOrMore alpha)


host : Parser String
host =
    separatedBy (char '.') (stringWith (oneOrMore alphaNum))
        |> map (String.join ".")


port_ : Parser Int
port_ =
    char ':'
        |> followedBy int


path : Parser String
path =
    stringWith (oneOrMore (except (oneOf [ char '?', char '#' ])))
        |> orElse (succeed "/")


query : Parser String
query =
    char '?'
        |> followedBy (stringWith (zeroOrMore (except (char '#'))))


fragment : Parser String
fragment =
    char '#'
        |> followedBy (stringWith (until end anyChar))
