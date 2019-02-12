﻿namespace Assignment02

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