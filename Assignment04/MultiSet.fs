module MultiSet

    type MultiSet<'a> when 'a : comparison = 
        | M of Map<'a, uint32>
        override q.ToString() =
            let (M m) = q
            let s = Map.fold (fun state key value -> sprintf "%s(%A, #%d), " state key value) "" m
            "{" + s.Substring(0, s.Length - 2) + "}"

    let empty :MultiSet<'a> = M Map.empty

    let isEmpty (M set : MultiSet<'a>) = Map.isEmpty set

    let size (M set : MultiSet<'a>) = Map.fold (fun state key value -> state + value) 0u set

    let contains key (M set : MultiSet<'a>) = set.ContainsKey key

    let numItems key (M set : MultiSet<'a>) = Map.find key set

    let add key num (M set : MultiSet<'a>) =
        if contains key (M set) then
            let prevNum = numItems key (M set)
            M(Map.add key (num + prevNum) set)
        else
            M(Map.add key num set)

    let addSingle key set = add key 1u set

    let remove key num (M set : MultiSet<'a>) =
        if contains key (M set) then
            let prevNum = numItems key (M set)
            if num > prevNum then
                M(Map.remove key set)
            else
                M(Map.add key (prevNum - num) set)
        else
            M(set)

    let removeSingle key set = remove key 1u set

    let fold f acc (M set : MultiSet<'a>) = Map.fold f acc set

    let foldBack f (M set : MultiSet<'a>) acc = Map.foldBack f set acc

    let map f (M set : MultiSet<'a>) =
        set
        |> Map.toList
        |> List.map (fun (key, value) -> (f key, value))
        |> List.fold (fun state (key, value) -> add key value state) empty

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

    let union (M a : MultiSet<'a>) (M b : MultiSet<'a>) = fold (fun state key value -> add key value state) (M a) (M b)

    let subtract (M a : MultiSet<'a>) (M b : MultiSet<'a>) = fold (fun state key value -> remove key value state) (M a) (M b)

    let intersection (M a : MultiSet<'a>) (M b : MultiSet<'a>) =
        fold (
                 fun state key value ->
                     if contains key (M b) then
                         add key (min value (numItems key (M b))) state
                     else
                         state
             ) empty (M a)