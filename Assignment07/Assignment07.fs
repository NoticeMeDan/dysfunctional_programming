module Assignment7
open System

type Error = 
    | VarNotFound of string
    | DivisionByZero

type Result<'a, 'b>  =
    | Success of 'a
    | Failure of 'b   

type aExp =          (* arithmetical expressions *)
| N of int           (* numbers *)
| V of string        (* variables *)
| Add of aExp * aExp (* addition *)    
| Mul of aExp * aExp (* multiplication *)
| Sub of aExp * aExp (* subtracting *)
| Div of aExp * aExp (* division *)
| Inc of string

type bExp =          (* boolean expressions *)
| TT                 (* true *)
| FF                 (* false *)
| Eq of aExp * aExp  (* numeric equality *)
| Lt of aExp * aExp  (* numeric less than *)
| Neg of bExp        (* boolean not *)
| Con of bExp * bExp (* boolean conjunction *)

type stm =                (* statements *)
| Declare of string
| Ass of string * aExp    (* variable assignment *)
| Skip                    (* nop *)
| Seq of stm * stm        (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else statement *)
| While of bExp * stm     (* while statement *)
| Block of stm

module ImpParser =
    open JParsec
    open TextParser
    open Parser
    // helper functions:
    let unop op p = (op .>> spaces) >>. p
    let binop op p1 p2 = p1 .>> (spaces .>>. op .>>. spaces) .>>. p2
    let identifier = 
         letterChar .>>. many alphaNumeric |>> 
         (fun (x, xs) -> x :: xs |> charListToStr) <?> "identifier"


    // parser A
    let TermParse, tref = createParserForwardedToRef<TextInputState, aExp>()
    let ProdParse, pref = createParserForwardedToRef<TextInputState, aExp>()
    let AtomParse, aref = createParserForwardedToRef<TextInputState, aExp>()

    let NParse   = pint |>> N <?> "Int"
    let VParse   = identifier |>> V <?> "Var"

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    do pref := choice [MulParse; DivParse; AtomParse]

    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul(N (-1), x)) <?> "Neg"
    let IncParse = unop (pchar '+' .>>. pchar '+') identifier |>> Inc <?> "Inc"
    let ParParse = between (pchar '(' .>>. spaces) TermParse (spaces .>>. pchar ')')
    do aref := choice [NegParse; IncParse; ParParse; NParse; VParse]

    let AexpParse = TermParse 

    (* The rest of your parser goes here *)
    let dis (b1, b2) = Neg (Con (Neg b1, Neg b2))
    let lte b = dis (Lt b, Eq b)

    let bExpParse, bRef = createParserForwardedToRef<TextInputState, bExp>()
    let compareParse, cRef = createParserForwardedToRef<TextInputState, bExp>()
    let bNegParse, nRef = createParserForwardedToRef<TextInputState, bExp>()

    let disjunctParse = binop (pchar '\\' .>>. pchar '/') compareParse bExpParse |>> dis <?> "greater than or equal"
    let conjunctParse = binop (pchar '/' .>>. pchar '\\') compareParse bExpParse |>> Con <?> "greater than or equal"
    do bRef := choice [conjunctParse; disjunctParse; compareParse]

    let negParse = unop (pchar '~') bNegParse |>> Neg <?> "Not"
    let trueParse = (pstring "true") |>> (fun _ -> TT) <?> "true"
    let falseParse = (pstring "false") |>> (fun _ -> FF) <?> "false"
    let parenParse = between (pchar '(' .>>. spaces) bExpParse (spaces .>>. pchar ')')
    do nRef := choice[negParse; trueParse; falseParse; parenParse]

    let ltParse = binop (pchar '<') AexpParse AexpParse |>> Lt <?> "lt"
    let lteParse = binop (pchar '<' .>>. pchar '=') AexpParse AexpParse |>> lte <?> "lte"
    let gtParse = binop (pchar '>') AexpParse AexpParse |>> lte |>> Neg <?> "gt"
    let gteParse = binop (pchar '>' .>>. pchar '=') AexpParse AexpParse |>> Lt |>> Neg <?> "gte"
    let eqParse = binop (pchar '=') AexpParse AexpParse |>> Eq <?> "equal"
    let notEqParse = binop (pchar '<' .>>. pchar '>') AexpParse AexpParse |>> Eq |>> Neg <?> "not equal"
    do cRef := choice [eqParse; notEqParse; ltParse; lteParse; gtParse; gteParse; bNegParse]
    //7.2
    let tspace parser = parser .>>  spaces

    //7.3
    let delimParse, delimRef = createParserForwardedToRef<TextInputState, stm>()
    let statementParse, statementRef = createParserForwardedToRef<TextInputState, stm>()
    
    let seqparse = binop (pchar ';') statementParse delimParse |>> Seq <?> "Seqment"    
    do delimRef := choice [seqparse; statementParse]
    
    let collapse p = (fst (fst p), snd (fst p), snd p)
    let assParser = binop (spaces.>>.pstring ":=".>>. spaces) identifier AexpParse |>> Ass <?> "Seqment"
    let declareParser = unop (spaces.>>.pstring "declare ".>>. spaces) identifier |>> Declare <?> "Seqment"

    let iteParse = (pstring "if")  |> tspace
                            >>. bExpParse |> tspace
                            .>> (pstring "then")  |> tspace
                            .>>. statementParse  |> tspace
                            .>> (pstring "else") |> tspace
                            .>>. statementParse  |> tspace
                            |>> (fun ((b, s1), s2) -> ITE (b,s1,s2) ) 
                            <?> "If then else"
                            
    let itParse = (pstring "if")  |> tspace
                       >>. bExpParse  |> tspace
                       .>> (pstring "then")  |> tspace
                       .>>. statementParse  |> tspace
                       |>> (fun (b, s) -> ITE (b,s, Skip) ) 
                       <?> "If then"
                       
    let wParse = between (pstring "while"  .>>. spaces .>>. pchar '(' .>>. spaces)
                       (bExpParse  |> tspace) ( spaces .>>. pchar ')' .>>. spaces)
                       .>>. statementParse  |> tspace
                       |>> (fun (b, s) -> While (b,s) ) 
                       <?> "While"
    
    let rParse = between (pstring "repeat" |> tspace) (statementParse |> tspace) (spaces)
                       .>> (pstring "until")  |> tspace
                       .>>. bExpParse |> tspace
                       |>> (fun (s, b) -> Seq(s, While(Neg b, s)) ) 
                       <?> "While"

    let blockParse = between (pstring "{" .>>. spaces) (tspace delimParse) (spaces .>>. pchar '}') |>> Block <?> "block"

    do statementRef := choice [assParser; iteParse; itParse; wParse; blockParse; rParse; declareParser]

    let stmParse = (tspace delimParse);

module ImpEval =
    type state = Map<string, int>
    
    type 'a T = state -> Result<('a*state),Error>
    
    let bind f (a :'a T) : 'b T =
        fun s -> 
            match a s with
            | Success (x, s1) -> f x s1
            | Failure e -> Failure e

    let (>>=) x (f: 'a -> 'b T) = bind f x
                
    let ret x = fun s -> Success (x, s)
    let switch f x  = ret (f x)
    let fail err = fun _ -> Failure err
    let find x = 
        fun s -> 
            match Map.tryFind x s with
            | Some v -> ret v s
            | None -> Failure (VarNotFound x)

    let set x v = fun s -> ret () (Map.add x v s)
    let binop f a b = a >>= fun x -> b >>= switch (f x)    
    let unop f a = a  >>= switch f
    let div a b = a >>= fun x -> b >>= fun y -> if y <> 0 then ret (x / y) else fail DivisionByZero
    
    let rec A =
        function
        | N n -> ret n
        | V x -> find x 
        | Add (a1, a2) -> binop ( + ) (A a1) (A a2)
        | Sub (a1, a2) -> binop ( - ) (A a1) (A a2)
        | Mul (a1, a2) -> binop ( * ) (A a1) (A a2)
        | Div (a1, a2) -> div (A a1) (A a2)
        | Inc x ->
            find x >>=
            fun v -> set x (v + 1) >>= 
                     fun _ -> ret (v + 1)
    
    let rec B =
        function
        | TT -> ret true
        | FF -> ret false
        | Eq(a1, a2) -> binop ( = ) (A a1) (A a2)
        | Lt(a1, a2) -> binop ( < ) (A a1) (A a2)
        | Neg b1 -> unop not (B b1)
        | Con(b1, b2) -> binop ( && ) (B b1) (B b2)

    let update = Map.add
    
    let rec I (stm:stm) : unit T=
        match stm with
        | Skip -> ret ()
        | Ass (k, a1) -> (A a1) >>= set k
        | Seq (st1, st2) -> I st1 >>= (fun _ -> I st2)
        | ITE (b1, st1, st2) -> (B b1) >>= (fun b -> if b then I st1 else I st2)
        | While (b1, st1) -> (B b1) >>= (fun b -> if b then I (Seq (st1, While(b1, st1))) else I Skip)
        | Block s -> I s
        

module ImpEval2 =
        open JParsec
        (* Exercise 7.4 goes here *)

        type state = Map<string, int> list
        
        let bind f a =
            fun (s:state) ->
                match a s with
                | Success(x, s1) -> f x s1
                | Failure e -> Failure e

        let (>>=) x f = bind f x

        let ret x = fun s -> Success(x, s)
        let switch f x = ret (f x)
        let fail err = fun _ -> Failure err

        let find x = fun (s:state) ->
            let rec aux (inner:state) (outer:state) =
                match outer with
                | [] -> Failure(VarNotFound x)
                | curr::outer ->
                    match Map.tryFind x curr with
                    | Some v -> ret v s
                    | None -> aux (inner@[curr]) outer
            aux [] s
           
        let set x v = fun (s:state) ->
            let rec aux (inner:state) (outer:state) =
                match outer with
                | [] -> Failure(VarNotFound x)
                | curr::outer ->
                    match Map.tryFind x curr with
                    | Some v_old -> ret () (inner@(Map.add x v curr)::outer)
                    | None -> aux (inner@[curr]) outer
            aux [] s

        let declare x = fun (s:state) ->
            match s with
            | [single] -> ret () [Map.add x 0 single]
            | top::rest -> ret () ([Map.add x 0 top]@rest)
            | [] -> ret () []
            
        let push = fun (s:state) ->
            match s with
            | [] -> ret () [Map<string,int>[]]
            | lst -> ret () (Map<string,int>[]::lst)
        
        let pop = fun (s:state) ->
            match s with
            | [] -> ret () []
            | inner::outers -> ret () outers

        let binop f a b = a >>= fun x -> b >>= switch (f x)

        let div a b =
            a >>=
            fun x -> b >>=
                     fun y ->
                     if y <> 0 then
                        ret (x / y)
                     else
                         fail DivisionByZero

        let rec A =
            function
            | N n -> ret n
            | V x -> find x
            | Add(a1, a2) -> binop (+) (A a1) (A a2)
            | Sub(a1, a2) -> binop (-) (A a1) (A a2)
            | Mul(a1, a2) -> binop (*) (A a1) (A a2)
            | Div(a1, a2) -> div (A a1) (A a2)
            | Inc x ->
                find x >>=
                fun v -> set x (v + 1) >>=
                         fun _ -> ret (v + 1)

        let rec B =
            function
            | TT -> ret true
            | FF -> ret false
            | Eq(a1, a2) -> binop (=) (A a1) (A a2)
            | Lt(a1, a2) -> binop (<) (A a1) (A a2)
            | Neg b -> B b >>= fun x -> ret (not x)
            | Con(b1, b2) -> binop (&&) (B b1) (B b2)

        let rec I =
            function
            | Ass(x, a) -> A a >>= fun v -> set x v
            | Declare(x) -> declare x
            | Skip -> ret()
            | Seq(stm1, stm2) -> I stm1 >>= fun () -> I stm2
            | ITE(b, stm1, stm2) -> B b >>= fun test -> if (test) then I(Block(stm1)) else I(Block(stm2))
            | While(b, stm) -> B b >>= fun test -> if (test) then I(Seq(Block(stm), While(b, stm))) else ret ()
            | Block(stm) -> push >>= (fun () -> I stm >>= (fun () -> pop))