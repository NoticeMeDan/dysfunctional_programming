module MultiSet
    /// A multiset is a set that can contain duplicate elements, so set's such as \{x, y, x\}{x,y,x} are perfectly valid

    type MultiSet<'a when 'a : comparison>

    /// creates an empty set
    val empty : MultiSet<'a>
    /// given a set s, return true if a s is empty, and false otherwise.
    val isEmpty : MultiSet<'a> -> bool
    /// given a set s, return the size of s
    val size : MultiSet<'a> -> uint32
    /// given an element a and a set s, return true if a is an element of s and false otherwise.
    val contains : 'a -> MultiSet<'a> -> bool
    ///given an element a and a set s, return how many occurrences of a there are in s.
    val numItems : 'a -> MultiSet<'a> -> uint32
    ///given an element a, a number n, and a set s, return the set that has a added to s n times.
    val add : 'a -> uint32  -> MultiSet<'a>  -> MultiSet<'a>
    ///given an element a and a set s, return s with a single nstance of a added to it.
    val addSingle : 'a -> MultiSet<'a> -> MultiSet<'a>
    /// given an element a, a number n, and a set s, return the set with n occurrences of a from s removed
    /// (remember that a set cannot have fewer than 0 entries of an element).
    /// If s does not contain n occurrences of a then the result should contain no occurrences of a.
    val remove : 'a -> uint32 -> MultiSet<'a> -> MultiSet<'a>
    /// given an element a and a set s return the set where a single occurrence of s has been removed. If s does not contain a then return s.
    val removeSingle : 'a -> MultiSet<'a> -> MultiSet<'a>
    /// given a folding function f, an accumulator acc and a set {(a1,x1), ..., (an,xn)},
    /// return f (... (f acc a1 x1) ...) an xn).
    /// Note that the folder function f takes the number of occurrences
    /// of each element as an argument rather than calling f that number of times.
    /// It is up to the user of the fold method to determine if he wants that type of behaviour
    /// and code his folder function accordingly if so (this is better practice, and it makes your life easier).
    val fold : ('a -> 'b -> uint32 -> 'a) -> 'a -> MultiSet<'b> -> 'a
    val foldBack : ('a -> uint32 -> 'b -> 'b) -> MultiSet<'a> -> 'b -> 'b
    /// given a list lst returns a set containing exactly the elements of lst
    val ofList : 'a list -> MultiSet<'a>
    /// given a set {(a1, x1), ..., (an, xn)} returns [a1; ..(x1 times)..; a1; ...; an; ..(xn times)..; an]
    val toList : MultiSet<'a> -> 'a list
    val map : ('a -> 'b) -> MultiSet<'a> -> MultiSet<'b>
    val union : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val subtract : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>
    val intersection : MultiSet<'a> -> MultiSet<'a> -> MultiSet<'a>