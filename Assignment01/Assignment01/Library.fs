namespace Assignment01

module Thingies =
    // 1.1
    let sqr num = num * num
    
    // 1.2
    let pow x n = System.Math.Pow(x, n)
    
    // 1.3
    let g n = n + 4
    
    // 1.4
    let h x y = System.Math.Sqrt(x * x + y * y)
    
    // 1.5
    let rec f = function
        | 0 -> 0
        | n -> n + f (n - 1)
        
    // 1.6
    let rec fib = function
        | 0 -> 0
        | 1 -> 1
        | n -> fib (n - 1) + fib (n - 2)

    // 1.7
    let rec sum (m, n) =
        match (m, n) with
        | (m, 0) -> m
        | (m, n) -> m + n + sum (m, n-1)
        
    // 1.8
    // float * int
    // int
    // float
    // (float * int -> float) * (int -> int)
    
    // 1.9
    
    //       | a    |-> 5                      |  
    // env = | f a  |-> 6 |
    //       | g b  |-> "the add six function" |