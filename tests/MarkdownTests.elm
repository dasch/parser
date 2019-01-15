module MarkdownTests exposing (suite)

import Examples.Markdown exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)


input : String
input =
    """
# This is an example of Markdown

1. One
2. Two
3. Three

You can have *different* kinds of __emphasis__!

* And
* Unordered
* Lists!
"""


suite : Test
suite =
    describe "Markdown"
        [ describe "parse"
            [ test "parses Markdown documents" <|
                \_ ->
                    let
                        expected : Document
                        expected =
                            [ Heading 1 [ Text "This is an example of Markdown" ]
                            , OrderedList
                                [ Paragraph [ Text "One" ]
                                , Paragraph [ Text "Two" ]
                                , Paragraph [ Text "Three" ]
                                ]
                            , Paragraph
                                [ Text "You can have "
                                , Emphasis "different"
                                , Text " kinds of "
                                , StrongEmphasis "emphasis"
                                , Text "!"
                                ]
                            , UnorderedList
                                [ Paragraph [ Text "And" ]
                                , Paragraph [ Text "Unordered" ]
                                , Paragraph [ Text "Lists!" ]
                                ]
                            ]
                    in
                    parse input
                        |> Expect.equal (Ok expected)
            ]
        , test "inline" <|
            \_ -> expectMarkdown [ Paragraph [ Text "hello ", Emphasis "world" ] ] "hello *world*"
        , test "heading" <|
            \_ -> expectMarkdown [ Heading 2 [ Text "hello" ] ] "## hello"
        ]


expectMarkdown expected string =
    Expect.equal (Ok expected) (parse string)
