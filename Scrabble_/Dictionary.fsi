module Dictionary

    type Dictionary
    
    /// Given a string s containing all of the letters of the alphabet that we use in the dictionary, 
    /// returns an empty dictionary that expects to be populated with words using only letters from this alphabet.
    val empty : string -> Dictionary
    
    /// Given a string s and a dictionary dict adds the word s to dict. 
    /// This function must not be (but can be) functional, i.e. you are allowed to use destructive updates
    /// within dict that actually changes the value of the argument passed to the function
    val insert : string -> Dictionary -> Dictionary
    
    /// given a string s and a dictionary dict returns true if s is in dict and false otherwise.
    val lookup : string -> Dictionary -> bool

