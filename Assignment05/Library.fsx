// 5.1
let sum m n =
    let rec loop x acc =
        match x with
        | 0 -> acc + m
        | _ -> loop (x - 1) (acc + m + x)
    loop n 0

// 5.2
let length lst =
    let rec loop x acc =
        match x with
        | [] -> acc
        | x :: xs -> loop xs (acc + 1)
    loop lst 0
    
// 5.3
(*
    let rec foldBack folder lst acc =
        match lst with
        | []      -> acc
        | x :: xs -> folder x (foldBack folder xs acc)
*)
    
let foldBack folder lst acc =
    let rec aux cont =
        function
        | [] -> cont acc
        | x :: xs -> aux (fun prev -> cont (folder x prev)) xs
    aux id lst

// 5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)

    aux 1 x

let factC x =
    let rec aux x f =
        if x = 0 then
            f()
        else
            aux (x - 1) (fun () -> x * f())

    aux x (fun () -> 1)

//    factA 16
//    Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
//    val it : int = 2004189184
//
//    factC 16
//    Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
//    val it : int = 2004189184
//
//    According to my machine, there is no difference between using an Accumulator and a Continuation.

// 5.5
let fibW x =
    let mutable res1 = 0
    let mutable res2 = 1
    let mutable i = 1
    while (i <= x) do
        let temp = res1
        res1 <- res2
        res2 <- temp + res2
        i <- i + 1
    res1

let fibA =
    let rec aux res1 res2 =
        function
        | 0 -> res1
        | x -> aux res2 (res2 + res1) (x - 1)
    aux 0 1

let fibC =
    let rec aux c =
        function
        | x when x < 2 -> c x
        | x -> aux (
                       fun res1 -> aux (fun res2 -> c (res1 + res2)) (x - 1)
                   ) (x - 2)
    aux id


//    fibW 40
//    Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
//    val it : int = 102334155
//    
//    fibA 40
//    Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
//    val it : int = 102334155
//    
//    fibC 40
//    Real: 00:00:05.448, CPU: 00:00:05.476, GC gen0: 2561, gen1: 0
//    val it : int = 102334155

// 5.6
// The function bigListK is not tail recursive
// the reason being (fun res -> 1 :: c res) (which will also just return a list of 1's)
// there is additional work after the recursive call.
// A solution to this would be (fun res -> c(1::res)
// now c is the last function called with no additional work afterwards
let rec bigListK c =
    function
    | 0 -> c []
    | n -> bigListK (fun res -> c(1::res)) (n - 1)
    
// 5.7
type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let emptyTree = Leaf


(* balancedTree x: creates a balanced tree with 2^x elements *)
let balancedTree x =
    let rec aux offset =
        function
        | 1 -> Leaf
        | x -> 
            let v = x / 2
            Node (offset + v, aux offset v, aux (offset + v) v)

    aux 0 (2.0 ** float x  |> int)

let insert x =
    let rec aux c =
        function
        | Leaf                      -> c (Node (x, Leaf, Leaf))
        | Node (y, l, r) when x < y -> aux (fun t -> c (Node (y, t, r))) l 
        | Node (y, l, r)            -> aux (fun t -> c (Node (y, l, t))) r 

    aux id

(* unbalancedRight x : creates a binary tree containing x elements that is 
    unbalanced to the right *)
let unbalancedRight = 
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (Node (x, Leaf, acc)) (x - 1)

    aux Leaf

(* unbalancedLeft x : creates a binary tree containing x elements that is 
    unbalanced to the left *)
let unbalancedLeft x =
    let rec aux acc =
        function
        | y when y > x -> acc
        | y            -> aux (Node (y, acc, Leaf)) (y + 1)

    aux Leaf 1

// 5.8
let getOdd n = 2*n-1
let odds = Seq.initInfinite (fun i -> getOdd i)

// 5.9
let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1)
    
let facts = Seq.initInfinite (fun i -> factorial i)
    
