module Dictionary

    type Dictionary
    
    val empty : string -> Dictionary
    val insert : string -> (Dictionary -> Dictionary)
    val lookup : string -> Dictionary -> bool