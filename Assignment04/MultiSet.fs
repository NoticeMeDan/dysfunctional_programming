module Assignment04.MultiSet

    type MultiSet<'a> when 'a : comparison = Map<'a, uint32>
    
    let empty :MultiSet<'a> = Map.empty<'a, uint32>
    
    let isEmpty (set : MultiSet<'a>) = Map.isEmpty set