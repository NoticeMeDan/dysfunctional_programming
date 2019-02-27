module Assignment04.Dictionary

type Dictionary =
      | Leaf of Dictionary list
      | Node of char * Dictionary list * bool
   
   let empty (s : string) = Leaf([])
   
   let insert (s : string)  =
      let charExists c =
         function
         | Leaf(_) -> false
         | Node(v, _, _) -> v = c

      let rec insertChars (chars: char list) =
         function
         | Leaf(list) when List.exists(charExists chars.Head) list = false -> Leaf(list @ [(Node(chars.Head, [], false) |> insertChars chars)])
         | Leaf(list) -> Leaf(List.map (fun node -> insertChars chars node) list)
         
         | Node(c, list, b) when c = chars.Head && chars.Tail = [] -> Node(c, list, true)
         | Node(c, list, b) when c = chars.Head && list = [] -> Node(c, [(Node(chars.Tail.Head, [], false) |> insertChars chars.Tail)], b)
         | Node(c, list, b) when c = chars.Head && List.exists(charExists chars.Tail.Head) list = false -> Node(c, list @ [(Node(chars.Tail.Head, [], false) |> insertChars chars.Tail)], b)
         | Node(c, list, b) when c = chars.Head -> Node (c, List.map (fun node -> insertChars chars.Tail node) list, b) 
         | Node(c, list, b) -> Node(c, list, b)
      
      Seq.toList s |> insertChars
   
   let lookup (s : string) (dict: Dictionary) =
      let characters = Seq.toList s
       
      let rec search (chars: char list) =
         function
         | Leaf(list) when list = [] -> false
         | Leaf(list) -> list |> List.exists (fun node -> search chars node) 
         | Node(c, list, b) when c = chars.Head && chars.Tail = [] -> b
         | Node(c, list, b) when c = chars.Head && list = [] -> false
         | Node(c, list, b) when c = chars.Head -> list |> List.exists (fun node -> search chars.Tail node)
         | Node(c, list, b) -> false
       
      search characters dict