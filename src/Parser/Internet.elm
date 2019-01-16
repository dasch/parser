module Parser.Internet exposing (url)

{-| Parsers related to Internet concepts such as URLs, email addresses, etc.

@docs url

-}

import Parser exposing (..)
import Parser.Common exposing (..)
import Url exposing (Url)


url : Parser Url
url =
    let
        protocol : Parser Url.Protocol
        protocol =
            oneOf
                [ string "https://" |> map (always Url.Https)
                , string "http://" |> map (always Url.Http)
                ]

        host : Parser String
        host =
            until (oneOf [ char ':', char '/' ]) anyChar
                |> stringWith

        port_ : Parser Int
        port_ =
            char ':'
                |> followedBy int

        path : Parser String
        path =
            until (oneOf [ ignored (char '?'), ignored (char '#'), end ]) anyChar
                |> orElse (succeed [])
                |> stringWith

        query : Parser String
        query =
            char '?'
                |> followedBy (until (oneOf [ ignored (char '#'), end ]) anyChar)
                |> stringWith

        fragment : Parser String
        fragment =
            char '#'
                |> followedBy (until end anyChar)
                |> stringWith

        ignored : Parser a -> Parser ()
        ignored parser =
            map (always ()) parser
    in
    into Url
        |> grab protocol
        |> grab host
        |> grab (maybe port_)
        |> grab path
        |> grab (maybe query)
        |> grab (maybe fragment)
