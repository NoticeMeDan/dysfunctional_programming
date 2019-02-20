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
        

    (* Exercise 3.9 *)

    (* TODO: complete these types and functions (we use mk<Name> to create an interface to the testing environment). *)

    (* type prop ... *)
    type prop =
        | Atom of string
        | Neg of prop
        | Conj of prop * prop
        | Disj of prop * prop

    (* TODO: create mkAtom : string -> prop *)
    let mkAtom x = Atom x

    (* TODO: create mkNeg : prop -> prop *)
    let mkNeg x = Neg x

    (* TODO: create mkConj : prop -> prop -> prop *)
    let mkConj x y = Conj(x,y)

    (* TODO: create mkDisj : prop -> prop -> prop *)
    let mkDisj x y = Disj(x,y)

    let mkImpl p q = mkDisj (mkNeg p) q
    let mkBiImpl p q = mkConj (mkImpl p q) (mkImpl q p)

    (* TODO: create nnf : prop -> prop *)
    let rec nnf = function
        | Neg(Conj(x,y))     -> Disj(nnf(Neg x), nnf(Neg y))
        | Neg(Disj(x,y))     -> Conj(nnf(Neg x), nnf(Neg y))
        | Neg(Neg(x))        -> nnf x
        | Conj(x,y)          -> Conj(nnf x, nnf y)
        | Disj(x,y)          -> Disj(nnf x, nnf y)
        | prop               -> prop
        
    (* TODO: create cnf : prop -> prop *)
    let rec cnf =
        let rec inner = function
            | Disj(p,Conj(q,r)) -> Conj(inner (Disj(p,q)), inner (Disj(p,r)))
            | Disj(Conj(p,q),r) -> Conj(inner(Disj(p,r)), inner (Disj(q,r)))
            | Disj(p,q)         -> match (inner p, inner q) with
                                    | (_,Conj(_,_)) | (Conj(_,_),_) as z -> inner(Disj z)
                                    | _                                  -> Disj(p,q)
            | Conj(p,q)         -> Conj(inner p, inner q)
            | prop              -> prop
        nnf >> inner

    (* TODO: create tautology : prop -> bool *)
    let tautology =
            let rec inner1 atoms = function
                | Atom name      -> match Map.tryFind name atoms with
                                | None         -> (false, Map.add name false atoms)
                                | Some negated -> (negated, atoms)
                | Neg(Atom name) -> match Map.tryFind name atoms with
                                | None         -> (false, Map.add name true atoms)
                                | Some negated -> (not negated, atoms)
                | Disj(p,q)      -> let (isTaut, atoms2) = inner1 atoms p
                                    if isTaut then (isTaut, Map.empty)
                                    else inner1 atoms2 q    
            let rec inner2 = function
                | Conj(p,q) -> inner2 p && inner2 q
                | f         -> fst (inner1 Map.empty f)
            cnf >> inner2

    (* TODO: create postfipProp : prop -> string *)
    (* Use the following string encodings 
     * atoms   - simply the string used for the atom )
     * not p   - p !
     * p and q - p q /\
     * p or q  - p q \/ 
     *)
    //let postfixProp _ = failwith "Not implemented"
    let rec postfixProp = function
       |Atom x    -> x
       |Neg x     -> System.String.Format(@"{0} {1}", postfixProp x,"!")
       |Conj(p,q) -> System.String.Format(@"{0} {1} /\",postfixProp p,postfixProp q)
       |Disj(p,q) -> System.String.Format(@"{0} {1} \/",postfixProp p,postfixProp q)

    (* TODO: create simplify : prop -> option prop *)
    let simplify _ = true      