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
        | IT of bExp * stm        (* If then, without else*)
        | Repeat of stm * bExp    (* While, where condition is checked after first run *)

    let update = Map.add

    let rec I stm s =
      match stm with
      | Ass (x, a)         -> update (x) (A a s) s
      | Skip               -> s
      | Seq (stm1, stm2)   -> (I stm1 >> I stm2) s
      | ITE (bExp, stm1, stm2)-> if (B bExp s) then I stm1 s else I stm2 s
      | While (bExp, stm1) -> I (ITE (bExp, Seq(stm1, stm), Skip)) s
      | IT (bExp, stm)        -> if (B bExp s) then I stm s else I Skip s
      | Repeat (stm, bExp)    -> I(Seq (stm, While(bExp, stm))) s
      
     // 3.7
    type Fexpr =
    | Const of float
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr
    
    let rec evalFexpr = function
        | Const x       -> float x
        | Add(f1,f2)    -> evalFexpr f1 + evalFexpr f2
        | Sub(f1,f2)    -> evalFexpr f1 - evalFexpr f2
        | Mul(f1,f2)    -> evalFexpr f1 * evalFexpr f2
        | Div(f1,f2)    -> evalFexpr f1 / evalFexpr f2
        | Sin f         -> sin <| evalFexpr f
        | Cos f         -> cos <| evalFexpr f
        | Log f         -> log <| evalFexpr f
        | Exp f         -> exp <| evalFexpr f

    // 3.8
    type Instruction =
        | ADD | SUB | MULT | DIV | SIN | COS | LOG | EXP | PUSH of float
    
    type Stack = float list
    
    
    let intpInstr instruction stack = 
        match instruction, stack with
        | ADD, x0::x1::xs    -> (x1 + x0)::xs
        | SUB, x0::x1::xs    -> (x1 - x0)::xs
        | MULT, x0::x1::xs   -> (x1 * x0)::xs
        | DIV, x0::x1::xs    -> (x1 / x0)::xs
        | SIN, x0::xs        -> (sin x0)::xs
        | COS, x0::xs        -> (cos x0)::xs
        | LOG, x0::xs        -> (log x0)::xs
        | EXP, x0::xs        -> (exp x0)::xs
        | PUSH(value), _     -> value::stack
        | _ -> failwith "something went wrong..."
    
    let intpProg instructions =
        let rec inner list (stack: float list) =
            match list with
            | [] -> stack.Head
            | top::rest -> inner rest (intpInstr top stack)
        inner instructions []
        
    let rec exprToInstrs expr =
        match expr with
        | Const(value) -> [PUSH(value)]
        | Add(x0, x1)  -> (exprToInstrs x0) @ (exprToInstrs x1) @ [ADD]
        | Sub(x0, x1)  -> (exprToInstrs x0) @ (exprToInstrs x1) @ [SUB]
        | Mul(x0, x1)  -> (exprToInstrs x0) @ (exprToInstrs x1) @ [MULT]
        | Div(x0, x1)  -> (exprToInstrs x0) @ (exprToInstrs x1) @ [DIV]
        | Sin(expr)    -> (exprToInstrs expr ) @ [SIN]
        | Cos(expr)    -> (exprToInstrs expr ) @ [COS]
        | Log(expr)    -> (exprToInstrs expr ) @ [LOG]
        | Exp(expr)    -> (exprToInstrs expr ) @ [EXP]
        
    let comp expr =
        let x = evalFexpr expr
        let y = intpProg <| exprToInstrs expr
        (x,y)      