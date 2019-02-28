namespace Assignment05

module Say =
    
    let sum m n =
        let rec loop x acc =
            match x with 
            | 0 -> acc
            | _ -> loop (x - 1) (acc + m + (x))
        loop n 0
        