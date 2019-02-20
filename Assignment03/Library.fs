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
        | Inc of string
    
    let binup eval a1 a2 state op =
        let eval1, state1 = eval a1 state
        let eval2, state2 = eval a2 state1
        (op eval1 eval2, state2)
                                    
    let unop eval a state op =
        let eval1, state1 = eval a state
        (op eval1, state1)
    
    let rec A e s =
        match e with
        | N n          -> (n, s)
        | V x          -> (Map.find x s, s)
        | Add (a1, a2) -> binup A a1 a2 s (+) 
        | Mul (a1, a2) -> binup A a1 a2 s (*) 
        | Sub (a1, a2) -> binup A a1 a2 s (-)
        | Inc x        -> let xval = Map.find x s
                          let newState = Map.add x (xval+1) s
                          (xval+1, newState)
                          
    type bExp =          (* boolean expressions *)
        | TT                 (* true *)
        | FF                 (* false *)
        | Eq of aExp * aExp  (* numeric equality *)
        | Lt of aExp * aExp  (* numeric less than *)
        | Neg of bExp        (* boolean not *)
        | Con of bExp * bExp (* boolean conjunction *)

    let rec B e s =
        match e with
        | TT          -> (true, s)
        | FF          -> (false, s)
        | Eq (a1, a2) -> binup A a1 a2 s (=) 
        | Lt (a1, a2) -> binup A a1 a2 s (<)
        | Neg (b1)    -> unop B b1 s not
        | Con (b1, b2) -> binup B b1 b2 s (&&)

    type stm =                (* statements *)
        | Ass of string * aExp    (* variable assignment *)
        | Skip                    (* nop *)
        | Seq of stm * stm        (* sequential composition *)
        | ITE of bExp * stm * stm (* if-then-else statement *)
        | While of bExp * stm     (* while statement *)
        | IT of bExp * stm        (* If then, without else*)
        | Repeat of stm * bExp    (* While, where condition is checked after first run *)

    let update = Map.add

    let rec I stm s =
        match stm with
        | Ass (x, a)         -> (A a s) ||> update (x)
        | Skip               -> s
        | Seq (stm1, stm2)   -> (I stm1 >> I stm2) s
        | ITE (bExp, stm1, stm2)->
            let (bool,state) = (B bExp s)
            if bool then I stm1 state else I stm2 state
        | While (bExp, stm1) -> I (IT (bExp, Seq(stm1, stm))) s
        | IT (bExp, stm)        -> I (ITE(bExp, stm, Skip)) s
        | Repeat (stm, bExp)    -> I(Seq (stm, While(bExp, stm))) s
      
    let factorial x =
        ITE (Neg (Lt (N x, N 0)),
             Seq (Seq (Ass ("result", N 1), Ass ("x", N 0)),
                  While (Neg (Eq (Add (N x, N 1), Inc "x")),
                         Ass ("result", Mul (V "result", V "x")))), 
             Skip)

    printfn "%A" (I (factorial 0) Map.empty |> Map.toList |> List.sortBy fst) 
