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
    // Tuples
    type BCur1 = BCur1 of int * int * int 
    
    let mkCur1 pounds shillings pence = BCur1(pounds, shillings, pence)
    
    let (.+.) m1 m2 =
        let (pounds1, shilling1, pence1) = m1
        let (pounds2, shilling2, pence2) = m2
        
        // Method the returns pence and gives the overflow to the next method
        let (pence, carry) =
            let sum = pence1 + pence2
            // Modulo to get leftover pence
            // Divide to get the "overflow"
            (sum % 12, sum / 12)
        
        // Method the returns shilling and gives the overflow to the next method
        let (shilling, carry) =
            let sum = shilling1 + shilling2 + carry // + the overflow from pence
            (sum % 20, sum / 20)
            
        let pounds = pounds1 + pounds2 + carry // + overflow from shilling
        // Final result
        (pounds, shilling, pence)
        
    let (.-.) m1 m2 =
        let (pounds1, shilling1, pence1) = m1
        let (pounds2, shilling2, pence2) = m2
        
        let (pence, borrow) =
            let difference = pence1 - pence2
            // If differences is < 0, we borrow
            if (difference < 0) then (difference + 12, 1) else (difference, 0)
        
        let (shilling, borrow) =
            let difference = shilling1 - shilling2 - borrow // 
            if (difference < 0) then (difference + 20, 1) else (difference, 0)
        
        let (pounds, borrow) =
            let difference = pounds1 - pounds2 - borrow
            if (difference < 0 ) then (difference + 10, 1) else (difference, 0)
        
        if (borrow>0) then failwith("you're out of money")
        (pounds, shilling, pence)
    
    let toString1 (BCur1(pounds, shillings, pence)) =
        sprintf "%d pounds, %d shillings, and %d pence" pounds shillings pence
        
    // Records
    type BCur2 = {
        pounds : int
        shilling : int
        pence : int
    }
    
    let mkCur2 pounds shilling pence = (pounds = pounds, shilling = shilling, pence = pence)
    
    let (..+..) m1 m2 =
        let (pounds1, shilling1, pence1) = (m1.pounds, m1.shilling, m1.pence)
        let (pounds2, shilling2, pence2) = (m2.pounds, m2.shilling, m2.pence)
        
        // Method the returns pence and gives the overflow to the next method
        let (pence, carry) =
            let sum = pence1 + pence2
            // Modulo to get leftover pence
            // Divide to get the "overflow"
            (sum % 12, sum / 12)
        
        // Method the returns shilling and gives the overflow to the next method
        let (shilling, carry) =
            let sum = shilling1 + shilling2 + carry // + the overflow from pence
            (sum % 20, sum / 20)
            
        let pounds = pounds1 + pounds2 + carry // + overflow from shilling
        // Final result
        {pounds = pounds; shilling = shilling; pence = pence}
        
    let (..-..) m1 m2 =
        let (pounds1, shilling1, pence1) = (m1.pounds, m1.shilling, m1.pence)
        let (pounds2, shilling2, pence2) = (m2.pounds, m2.shilling, m2.pence)
        
        let (pence, borrow) =
            let difference = pence1 - pence2
            // If differences is < 0, we borrow
            if (difference < 0) then (difference + 12, 1) else (difference, 0)
        
        let (shilling, borrow) =
            let difference = shilling1 - shilling2 - borrow // 
            if (difference < 0) then (difference + 20, 1) else (difference, 0)
        
        let (pounds, borrow) =
            let difference = pounds1 - pounds2 - borrow
            if (difference < 0 ) then (difference + 10, 1) else (difference, 0)
        
        if (borrow>0) then failwith("you're out of money")
    
        (pounds = pounds, shilling = shilling, pence = pence)
    
    let toString2 (x:BCur2) =
        sprintf "%d pounds, %d shillings, and %d pence" x.pounds x.shilling x.pence
    
    // Exercise 2.5
    type complex = float * float
    
    let mkComplex x y = complex (x, y)
    
    let (|+|) ((a, b): complex) ((c, d): complex) = complex (a + c, b + d)
    let (|-|) ((a, b): complex) ((c, d): complex) = (a, b) |+| (-c, -d)
    
    let (|*|) ((a, b): complex) ((c, d): complex) = complex ((a * c) - (b * d), (b * c) + (a * d))
        
    let (|/|) ((a, b): complex) ((c, d): complex) =
        let divisor = c ** 2. + d ** 2.
        (a, b) |*| (c / divisor, -d / divisor)
    
    // Exercise 2.6
    let rec altsum = function
        | [] -> 0
        | x0::xs -> x0 - altsum xs
        
    // Exercise 2.7
    let explode1 (str: string) = List.ofArray(str.ToCharArray())
    let rec explode2 (str: string) =
        match str with
        | "" -> []
        | x when x.Length = 1 -> [x.[0]]
        | x -> x.[0] :: explode2 (str.Remove(0, 1)) 
    
    // Exercise 2.8
    let implode (arr:char list) = List.foldBack (fun x acc -> string x + acc) arr ""
    let implodeRev (arr: char list) = List.fold (fun acc x -> string x + acc) "" arr
        
    // Exercise 2.9
    let toUpper (x: string)  = x.ToUpper()
    let toUpper1 (x: string) = (explode1 >> List.map System.Char.ToUpper >> implode) x
    let toUpper2 (x: string) = x |> explode1 |> List.map System.Char.ToUpper |> implode 
    
    // Exercise 2.10
    let rec palindrome1 (str: string) =
        match str.ToUpper() with
        | x when not (System.Char.IsLetter x.[0]) -> palindrome1 x.[1..]
        | x when not (System.Char.IsLetter x.[x.Length - 1]) -> palindrome1 x.[0..(x.Length - 2)]
        | x when x.Length = 1 -> true
        | x when x.Length = 2 -> x.[0] = x.[1]
        | x when not (x.[0] = x.[x.Length - 1]) -> false
        | x when x.[0] = x.[x.Length - 1] -> palindrome1 x.[1..(x.Length - 2)]
        | _ -> false
    
    let palindrome2 str =
        str
        |> explode1
        |> List.filter System.Char.IsLetter
        |> List.map System.Char.ToUpper
        |> (fun arr -> (implode arr) = (implode (List.rev arr)))
    
    // Exercise 2.11
    let rec ack ((m:int), (n:int)) =
        match (m,n) with
        | (m,n) when m.Equals 0 -> n + 1
        | (m,n) when m > 0 && n = 0 -> ack ((m-1),1)
        | (m,n) when m >= 0 && n > 0 -> ack ((m-1),ack (m,(n-1)))
        