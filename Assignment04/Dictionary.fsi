module Assignment04.Dictionary

    type Dictionary =
      | Leaf of Dictionary list
      | Node of char * Dictionary list * bool
    
    val empty : string -> Dictionary
    val insert : string -> (Dictionary -> Dictionary)
    val lookup : string -> Dictionary -> bool