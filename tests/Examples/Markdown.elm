module Examples.Markdown exposing (Document, parse)

import Parser exposing (..)
import Parser.Common exposing (..)


type alias Document =
    List Block


type Block
    = Paragraph (List Inline)


type Inline
    = Text String


parse : String -> Result String Document
parse input =
    Parser.parse input document


document : Parser Document
document =
    separatedBy (oneOrMore emptyLine) block
        |> ignore end


block : Parser Block
block =
    oneOf [ paragraph ]


paragraph : Parser Block
paragraph =
    oneOrMore (map Text nonEmptyLine)
        |> map Paragraph


emptyLine : Parser String
emptyLine =
    string "\n"


nonEmptyLine : Parser String
nonEmptyLine =
    oneOrMore (anyCharExcept '\n')
        |> map String.fromList
