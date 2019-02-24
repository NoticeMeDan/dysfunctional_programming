module Assignment04.MultiSet

    type MultiSet<'a> when 'a : comparison = Map<'a, uint32>
    
    let empty :MultiSet<'a> = Map.empty<'a, uint32>
    
    let isEmpty (set : MultiSet<'a>) = Map.isEmpty set
    
    let size (set: MultiSet<'a>) = Map.fold (fun state key value -> state + value) 0u set
    
    let contains key (set: MultiSet<'a>) = Map.containsKey key set
    
    let numItems key (set: MultiSet<'a>) = Map.find key set
    
    let add key num (set: MultiSet<'a>) =
        if contains key set then
            let prevNum = numItems key set
            Map.add key (num + prevNum) set
        else 
            Map.add key num set
    
    let addSingle key set = add key 1u set
    
    let remove key num (set: MultiSet<'a>) =
        if contains key set then
            let prevNum = numItems key set
            if num > prevNum then
                Map.remove key set
            else
                Map.add key (prevNum - num) set
        else
            set
    
    let removeSingle key set = remove key 1u set