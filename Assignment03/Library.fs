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