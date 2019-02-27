module Assignment04.MultiSet

    type MultiSet<'a> when 'a : comparison = 
        | M of Map<'a, uint32>
        override q.ToString() =
            match q with
            | M(map) -> sprintf "%s" ((Map.fold (fun state key value -> state + "(" + key.ToString() + ", #" + (string value)) "{" map) + "}")

    let empty :MultiSet<'a> = M Map.empty<'a, uint32>

    let isEmpty (M set : MultiSet<'a>) = Map.isEmpty set

    let size (M set : MultiSet<'a>) = Map.fold (fun state key value -> state + value) 0u set

    let contains key (M set : MultiSet<'a>) = Map.containsKey key set

    let numItems key (M set : MultiSet<'a>) = Map.find key set

    let add key num (M set : MultiSet<'a>) =
        if contains key (M set) then
            let prevNum = numItems key (M set)
            Map.add key (num + prevNum) set
        else
            Map.add key num set

    let addSingle key set = add key 1u set

    let remove key num (M set : MultiSet<'a>) =
        if contains key (M set) then
            let prevNum = numItems key (M set)
            if num > prevNum then
                Map.remove key set
            else
                Map.add key (prevNum - num) set
        else
            set

    let removeSingle key set = remove key 1u set

    let fold f acc (M set : MultiSet<'a>) = Map.fold f acc set

    let foldBack f acc (set : MultiSet<'a>) = Map.foldBack f acc set

    let map f (M set : MultiSet<'a>) = fold (fun state key value -> add (f key) value set) empty set

    let ofList (lst : 'a list) : MultiSet<'a> =
        lst
        |> List.groupBy (fun x -> x)
        |> List.map (fun (key, value) -> key, uint32 (List.length value))
        |> Map.ofList
        |> M

    let toList (M set : MultiSet<'a>) =
        set
        |> Map.toList
        |> List.collect (fun (key, value) -> List.replicate (int value) key)

    let union (M a : MultiSet<'a>) (M b : MultiSet<'a>) = fold (fun state key value -> add key value (M state)) a (M b)

    let subtract (M a : MultiSet<'a>) (M b : MultiSet<'a>) = fold (fun state key value -> remove key value (M state)) a (M b)

    let intersection (M a : MultiSet<'a>) (M b : MultiSet<'a>) =
        fold (
                fun state key value ->
                    if contains key b then
                        add key (min value (numItems key b)) state
                    else
                        state
            ) empty a