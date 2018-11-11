# dasch/parser

Easy to use text parsing library for Elm.

This library does not attempt to provide the fastest parsing API, nor does it allow for the most flexibility when it comes to error messages and contextual feedback. Rather, this library seeks to provide an intuitive, easy to use API for writing parsers that don't need to be crazy fast or provide detailed error messages.


## Examples

Let's say you want to parse URLs such as `https://hello.com:123/greetings?recipient=world#message` into a nice data type, e.g.

```elm
type alias Url =
    { protocol : String
    , host : String
    , port_ : Maybe Int
    , path : String
    , query : Maybe String
    , fragment : Maybe String
    }
```

In this case, we'd want a value that looks like this:

```elm
{ protocol = "https"
, host = "hello.com"
, port_ = Just 123,
, path = "/greetings"
, query = Just "recipient=world"
, fragment = Just "message"
}
```

You also want an error if the parsing fails.

Let's start by assuming we've already defined the individual pieces:

```elm
import Parser exposing (..)

-- Parser.Common has useful high level parsers.
import Parser.Common exposing (..)

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
```

There's already a lot of functionality here:

* `url` has type `Parser Url`, meaning that when it's run, it'll parse a `Url` value when successful.
* We're buiding a `Url`, and the `into` function starts a pipeline that allows for that. Any function can be used here as long as its arguments match the following `grab` lines.
* The `|>` (pipe) operator is used to feed values into the `Url` constructor.
* Each pipeline element can either `grab` or `ignore` the value returned by a parser.
* The `string` function allows creating a parser that matches an exact string).
* The `maybe` function allows _maybe_ matching a parser, meaning that the matching is optional and will succeed with `Nothing` if there's no match.

Now let's define each individual component. All of them are `Parser String`, so they'll succeed with a `String` value.

```elm
protocol : Parser String
protocol =
    stringWith (oneOrMore alpha)
```

Here we match one or more alphabetic characters, then map the characters to a `String`. We need to do this because `alpha` is a `Parser Char`, and `oneOrMore` turns a `Parser a` into a `Parser (List a)`. Therefore we'll end up with `Parser (List Char)`, but `stringWith` allows us to convert that into a `Parser String`.

```elm
host : Parser String
host =
    separatedBy (char '.') (stringWith (oneOrMore alphaNum))
        |> map (String.join ".")
```

Here we first use `separatedBy` to get all the components of the host. Each component matches a string with one or more alphanumeric characters, and each component is separated by `.`. If this matches we'll end up with a `Parser (List String)`. By using `map` we'll turn that into a `Parser String` that succeeds with each component joined with a `.` again.

```elm
port_ : Parser Int
port_ =
    char ':'
        |> followedBy int
```

Here we match a `:` followed by an integer, e.g. `42`. `followedBy` discards the value of the previous parser in the pipeline in favor of its argument's value, so we'll just get a `Parser Int`.

```elm
path : Parser String
path =
    stringWith (oneOrMore (except (oneOf [ char '?', char '#' ])))
        |> orElse (succeed "/")
```

We match a string with one or more characters that are _not_ one of `?` or `#`. If that fails, i.e. there's no path, we fall back to succeeding with `/` as the path using `orElse`.

```elm
query : Parser String
query =
    char '?'
        |> followedBy (stringWith (zeroOrMore (except (char '#'))))
```

We match the character `?` followed by a string with zero or more characters that are _not_ `#`.

```elm
fragment : Parser String
fragment =
    char '#'
        |> followedBy (stringWith (until end anyChar))
```

We match the character `#` followed by a string with zero or more of any character until the end of the parsed string.

Now we have a parser that matches most valid URLs, but how do we run it? Here's how:

```elm
parsedUrl : Result String Url
parsedUrl =
    Parser.parse "https://hello.com:123/greetings?recipient=world#message" url
```

`parse` takes an input string and a parser of type `Parse a` and returns a `Result String a`, with the `String` containing the error type.
