namespace Assignment01

module Thingies =
    // 1.1
    let sqr num = num * num

    // 1.2
    let pow x n = System.Math.Pow(x, n)

    // 1.3
    let g n = n + 4

    // 1.4
    let h (x,y) = System.Math.Sqrt(x * x + y * y)

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
        | (m, n) -> m + n + sum (m, n - 1)

    // 1.8

    // float * int
    // int
    // float
    // (float * int -> float) * (int -> int)

    // 1.9

    //       | a |-> 5                      |
    // env = | f |-> "the add one function" |
    //       | g |-> "the add six function" |

    // 1.10
    let dup str: string = str + str

    // 1.11
    let rec dupn (str : string) n =
        match (str, n) with
        | (str, 1) -> str
        | (str, _) -> str + dupn str (n - 1)
        
    // 1.12
    let timediff (h1, m1) (h2, m2) = (h2*60+m2) - (h1*60+m1)
    
    // 1.13
    let minutes (x, y) = timediff (0, 0) (x, y)
    
    // 1.14
    let rec pow_ (str : string, n) =
        match (str, n) with
        | (str, 1) -> str
        | (str, _) -> str + pow_ (str, n - 1)
    
    // 1.15
    let rec bin (n, k) =
        match (n, k) with
         | (n, 0) -> 1
         | (n, k) when k = n -> 1
         | (n, k) when n > k -> bin(n - 1, k - 1) + bin(n - 1, k)
  
    // 1.16
    // 1. int * int -> int
    // 2. When x >= 0
    // 3. 
    // f (2, 3)
    // ~> f (2-1, 2*3)
    // ~> f (2-1-1, 1*2*3)
    // ~> 6
    // 4. We have no clue - this is SWU not Mathematics
    
    // 1.17
    // 1. bool * int -> int
    // 2. This results in a stackoverflow, as fact -1 is evaluated immediately
    // 3. This results in 0, because fact -1 is never evaluated
    
    // 1.18
    let curry f = fun g -> fun h -> f (g, h)
    let uncurry f = fun (g, h) -> f g h