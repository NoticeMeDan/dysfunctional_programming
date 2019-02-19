namespace Assignment03

module Say =
    type 'a bintree when 'a : comparison =
    | Leaf
    | Node of ('a * 'a bintree * 'a bintree)
    
    let rec insert x =
        function 
        | Leaf                       -> Node (x, Leaf, Leaf)
        | Node (y, l, r) when x <= y -> Node (y, insert x l, r)
        | Node (y, l, r)             -> Node (y, l, insert x r)
    
    let rec inOrder =
        function
        | Leaf           -> []
        | Node (x, l, r) -> inOrder l @ x::inOrder r
     
    // 3.1
    let binarySort list = List.foldBack insert list Leaf |> inOrder
    
    // 3.2
    let rec mapInOrder f =
            function
            | Leaf -> Leaf
            | Node(x, l, r) -> 
                let temp = mapInOrder f l
                Node(f x, temp, mapInOrder f r)
    
    // 3.3
    let rec foldInOrder func init =
        function
        | Leaf -> init
        | Node(x, l, r) ->
            foldInOrder func init l
            |> (fun init -> func x init)
            |> (fun init -> foldInOrder func init r)
            
    // 3.4
    type aExp =          (* arithmetical expressions *)
        | N of int           (* numbers *)
        | V of string        (* variables *)
        | Add of aExp * aExp (* addition *)
        | Mul of aExp * aExp (* multiplication *)
        | Sub of aExp * aExp (* subtraction *)

    let rec A e s =
        match e with
        | N n          -> n
        | V x          -> Map.find x s
        | Add (a1, a2) -> A a1 s + A a2 s
        | Mul (a1, a2) -> A a1 s * A a2 s
        | Sub (a1, a2) -> A a1 s - A a2 s
        
    type bExp =          (* boolean expressions *)
        | TT                 (* true *)
        | FF                 (* false *)
        | Eq of aExp * aExp  (* numeric equality *)
        | Lt of aExp * aExp  (* numeric less than *)
        | Neg of bExp        (* boolean not *)
        | Con of bExp * bExp (* boolean conjunction *)

    let rec B e s =
        match e with
        | TT          -> true
        | FF          -> false
        | Eq (a1, a2) -> A a1 s = A a2 s
        | Lt (a1, a2) -> A a1 s < A a2 s
        | Neg (b1)    -> not (B b1 s)
        | Con (b1, b2) -> (B b1 s) && (B b2 s)

    type stm =                (* statements *)
        | Ass of string * aExp    (* variable assignment *)
        | Skip                    (* nop *)
        | Seq of stm * stm        (* sequential composition *)
        | ITE of bExp * stm * stm (* if-then-else statement *)
        | While of bExp * stm     (* while statement *)

    let update = Map.add

    let rec I e s =
      match e with
      | Ass (x, a)        -> update (x) (A a s) s
      | Skip              -> s
      | Seq (stm1, stm2)  -> (I stm1 >> I stm2) s
      | ITE (b,stm1, stm2) -> if (B b s) then I stm1 s else I stm2 s
      | While (b, stm)    -> if (B b s) then I stm s else s
      
    let factorial x =
        Seq (Ass("x", N x),
                ITE (Neg(Lt(V "x", N 0)),
                     Seq (Ass("result", N 1),
                          While(Lt(N 0, V "x"),
                                 Seq(Ass("result", Mul(V "result", V "x")),
                                      Ass("x", Sub(V "x", N 1))))),
                     Skip))

    let x = factorial 5
    printfn "%A" x
    printfn "%A" (I (factorial -5) Map.empty |> Map.toList |> List.sortBy fst)
    
    printfn "%A" (I (factorial 5) Map.empty |> Map.toList |> List.sortBy fst)