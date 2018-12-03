module Parser.MaybeCommon exposing (between, blankLine, blankLines, formatError, fromResult)

{-| Candidates for inclusion in Parser.Common and Parser.
-}

import Parser exposing (..)
import Parser.Common exposing (..)


blankLines : Parser ()
blankLines =
    zeroOrMore blankLine
        |> map (\_ -> ())


blankLine : Parser ()
blankLine =
    succeed ()
        |> ignore blanks
        |> ignore newline


between : Parser a -> Parser b -> Parser c -> Parser (List c)
between open close inner =
    succeed identity
        |> ignore open
        |> grab (until close inner)
        |> ignore close


fromResult : Parser (Result String a) -> Parser a
fromResult parser =
    let
        resultToParser result =
            case result of
                Ok v ->
                    succeed v

                Err err ->
                    fail err
    in
    parser
        |> andThen resultToParser


formatError : String -> Error -> String
formatError input { message, position } =
    let
        context =
            10

        slice =
            String.slice (position - context) (position + context) input
                |> Debug.toString

        cursor =
            position - context

        padding =
            String.repeat context " "
    in
    slice ++ "\n" ++ padding ++ "^ " ++ message
