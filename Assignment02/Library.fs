namespace Assignment02

module wee =
    // 2.1
    let rec downto1 n = 
        if n > 0
        then n::downto1(n-1)
        else []
    
    // 2.2
    let rec downto2 n =
        match n with
        | 0 -> []
        | n -> n::downto1(n-1)
