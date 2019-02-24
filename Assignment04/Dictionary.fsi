module Assignment04.Dictionary

    type Dictionary =
        | Leaf of string
        | Node of string * Dictionary * Dictionary
    
    val empty : string -> Dictionary
    val insert : string -> Dictionary -> Dictionary
    val lookup : string -> Dictionary -> bool