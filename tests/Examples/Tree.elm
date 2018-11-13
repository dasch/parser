module Examples.Tree exposing (..)

import Parser exposing (..)


type Tree
    = Leaf
    | Node Tree Tree


tree : Parser Tree
tree =
    oneOf [ leaf, node ]


leaf : Parser Tree
leaf =
    map (always Leaf) (char 'x')


node : Parser Tree
node =
    into Node
        |> ignore (char '@')
        |> grab (lazy (\_ -> tree))
        |> grab (lazy (\_ -> tree))
