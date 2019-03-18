module Assignment6

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
| Ass of string * aExp    (* variable assignment *)
| Skip                    (* nop *)
| Seq of stm * stm        (* sequential composition *)
| ITE of bExp * stm * stm (* if-then-else statement *)
| IT of bExp * stm        (* if-then statement *)
| While of bExp * stm     (* while statement *)
| Repeat of stm * bExp    (* repeat-statement *)

module E1 =
        
    type state = Map<string, int>

    let bind f a =
        fun s -> 
            match a s with
            | Success (x, s1) -> f x s1
            | Failure e -> Failure e

    let (>>=) x f = bind f x
                
    let ret x = fun s -> Success (x, s)
    let switch f x = ret (f x)
    let fail err = fun _ -> Failure err

    let find x = 
        fun s -> 
            match Map.tryFind x s with
            | Some v -> ret v s
            | None -> Failure (VarNotFound x)

    let set x v = fun s -> ret () (Map.add x v s)


    
    let binop f a b =
        a >>= fun x -> b >>= switch (f x)

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
        | Add (a1, a2) -> binop ( + ) (A a1) (A a2)
        | Sub (a1, a2) -> binop ( - ) (A a1) (A a2)
        | Mul (a1, a2) -> binop ( * ) (A a1) (A a2)
        | Div (a1, a2) -> div (A a1) (A a2)
        | Inc x ->
            find x >>=
            fun v -> set x (v + 1) >>=
                     fun _ -> ret (v + 1)

module E2 =

    let bind f a =
        fun s -> 
            match a s with
            | Success (x, s1) -> f x s1
            | Failure e -> Failure e

    let (>>=) x f = bind f x
                
    let ret x = fun s -> Success (x, s)
    let switch f x = ret (f x)
    let fail err = fun _ -> Failure err

    let find x = 
        fun s -> 
            match Map.tryFind x s with
            | Some v -> ret v s
            | None -> Failure (VarNotFound x)

    let set x v = fun s -> ret () (Map.add x v s)


    type MyResultBuilder() =
        member bld.Bind (a, f)  = bind f a 
        member bld.Return e     = ret e
        member bld.ReturnFrom e = e

    let eval = MyResultBuilder()

    let binop f a b =
        eval {let! x = a
              let! y = b
              return f x y}

    let div a b = 
        eval { let! x = a
               let! y = b
               if y <> 0 then 
                  return (x / y) 
               else
                  return! fail DivisionByZero}
    let rec A =
        function
        | N n -> eval {return n}
        | V x -> find x 
        | Add (a1, a2) -> binop ( + ) (A a1) (A a2)
        | Sub (a1, a2) -> binop ( - ) (A a1) (A a2)
        | Mul (a1, a2) -> binop ( * ) (A a1) (A a2)
        | Div (a1, a2) -> div (A a1) (A a2)
        | Inc x ->
            eval {
                let! v = find x
                do! set x (v + 1)
                return v + 1
            }
            
    // 6.4
    (*
        
    *)
        
    // 6.5