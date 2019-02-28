let sum m n =
    let rec loop x acc =
        match x with 
        | 0 -> acc + m
        | _ -> loop (x - 1) (acc + m + x)
    loop n 0
    
let length lst =
    let rec loop x acc  =
        match x with
        | [] -> acc
        | x::xs -> loop xs (acc + 1)
    loop lst 0
    
    