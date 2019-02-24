module Assignment04.Dictionary

    type Dictionary =
        | Leaf of string
        | Node of string * Dictionary * Dictionary