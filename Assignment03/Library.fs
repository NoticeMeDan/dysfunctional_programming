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

    let unop  f a s   = f (a s)
    let binop f a b s = f (a s) (b s)

    let rec A =
        function
        | N n          -> fun _ -> n
        | V x          -> Map.find x
        | Add (a1, a2) -> binop ( + ) (A a1) (A a2)
        | Mul (a1, a2) -> binop ( * ) (A a1) (A a2)
        | Sub (a1, a2) -> binop ( - ) (A a1) (A a2)

    type bExp =          (* boolean expressions *)
    | TT                 (* true *)
    | FF                 (* false *)
    | Eq of aExp * aExp  (* numeric equality *)
    | Lt of aExp * aExp  (* numeric less than *)
    | Neg of bExp        (* boolean not *)
    | Con of bExp * bExp (* boolean conjunction *)

    (* TODO: Write a function B : bExp -> state -> bool
             to evaluate boolean expressions (note that you will need to refer to A) *)


    let rec B _ = failwith "Not implementd"


    type stm =                (* statements *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nop *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let update = Map.add

    (* TODO: Write I : stm -> state -> state. 
             You can rewrite the skeleton if you want, but the signature must be the same *)


    let rec I =
      function 
      | Ass (x,a)         -> failwith "Not implemented" (* use update *)
      | Skip              -> failwith "Not implemented"
      | Seq (stm1, stm2)  -> failwith "Not implemented"
      | ITE (b,stm1,stm2) -> failwith "Not implemented"
      | While (b, stm)    -> failwith "Not implemented"