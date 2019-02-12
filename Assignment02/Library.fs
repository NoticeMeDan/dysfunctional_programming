namespace Assignment02

module wee =
    // Exercise 2.1
    let rec downto1 n = 
        if n > 0
        then n::downto1(n-1)
        else []
    
    let rec downto2 n =
        match n with
        | 0 -> []
        | n -> n::downto1(n-1)
        
    // Exercise 2.2
    let rec removeOddIdx =
        function
        | [] -> []
        | [x] -> [x]
        | x :: y :: xs -> x :: removeOddIdx xs
        
        
    // Exercise 2.3
    let rec combinePair t =
        match t with
        | [] -> []
        | [x1] -> []
        | x1::x2::tt -> (x1, x2) :: combinePair tt
    
    // Exercise 2.4
    
    // Exercise 2.5
    type complex = float * float
    
    let mkComplex x y = complex (x, y)
    
    let (|+|) ((a, b): complex) ((c, d): complex) = complex (a + c, b + d)
    let (|-|) ((a, b): complex) ((c, d): complex) = (a, b) |+| (-c, -d)
    
    let (|*|) ((a, b): complex) ((c, d): complex) = complex ((a * c) - (b * d), (b * c) + (a * d))
        
    let (|/|) ((a, b): complex) ((c, d): complex) =
        let divisor = c ** 2. + d ** 2.
        complex (a, b) |*| (c / divisor, -d / divisor)