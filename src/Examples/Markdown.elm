module Examples.Markdown exposing (Block(..), Document, Inline(..), parse)

import Parser exposing (..)
import Parser.Common exposing (..)


type alias Document =
    List Block


type Block
    = Paragraph (List Inline)
    | OrderedList (List Block)
    | UnorderedList (List Block)
    | Heading Int (List Inline)


type Inline
    = Text String
    | Emphasis String
    | StrongEmphasis String


parse : String -> Result Error Document
parse input =
    Parser.parse input document


document : Parser Document
document =
    until end block


block : Parser Block
block =
    zeroOrMore emptyLine
        |> followedBy (oneOf [ heading, orderedList, unorderedList, paragraph ])
        |> inContext "block"


surroundedBy : Parser a -> Parser b -> Parser c -> Parser c
surroundedBy before after inner =
    before
        |> followedBy inner
        |> andThen
            (\value ->
                after
                    |> followedBy (succeed value)
            )


inline : Parser (List Inline)
inline =
    let
        span : Parser Inline
        span =
            oneOf [ strongEmphasis, emphasis, text ]

        plain : Parser String
        plain =
            oneOrMore (except (oneOf [ newline, char '*', char '_' ]))
                |> stringWith

        text : Parser Inline
        text =
            map Text plain

        strongEmphasis : Parser Inline
        strongEmphasis =
            surroundedBy (string "__") (string "__") plain
                |> map StrongEmphasis

        emphasis : Parser Inline
        emphasis =
            surroundedBy (char '*') (char '*') plain
                |> map Emphasis
    in
    into identity
        |> grab (oneOrMore span)
        |> ignore (maybe newline)
        |> inContext "inline"


heading : Parser Block
heading =
    let
        headingForLevel : List Char -> Parser Block
        headingForLevel chars =
            let
                level =
                    List.length chars
            in
            if level == 0 then
                fail "not a heading"

            else
                oneOrMore blank
                    |> followedBy inline
                    |> map (Heading level)
    in
    oneOrMore (char '#')
        |> andThen headingForLevel
        |> inContext "heading"


paragraph : Parser Block
paragraph =
    oneOrMore inline
        |> map List.concat
        |> map Paragraph
        |> inContext "paragraph"


orderedList : Parser Block
orderedList =
    let
        marker =
            int
                |> followedBy (char '.')
                |> followedBy (oneOrMore blank)

        item : Parser Block
        item =
            marker
                |> followedBy inline
                |> map Paragraph
    in
    oneOrMore item
        |> map OrderedList


unorderedList : Parser Block
unorderedList =
    let
        marker =
            char '*'
                |> followedBy (oneOrMore blank)

        item : Parser Block
        item =
            marker
                |> followedBy inline
                |> map Paragraph
    in
    oneOrMore item
        |> map UnorderedList


emptyLine : Parser String
emptyLine =
    string "\n"


nonEmptyLine : Parser String
nonEmptyLine =
    oneOrMore (except (char '\n'))
        |> map String.fromList
