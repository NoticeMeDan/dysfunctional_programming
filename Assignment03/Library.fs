namespace Assignment03

module Say =
    // 3.1
    type 'a bintree = 
    | Leaf
    | Node of 'a * 'a bintree * 'a bintree

    let rec insert x =
        function
        | Leaf                       -> Node(x, Leaf, Leaf)
        | Node (y, l, r) when x <= y -> Node(y, insert x l, r)
        | Node (y, l, r)             -> Node(y, l, insert x r)
    
    let rec inOrder =
        function
        | Leaf           -> []
        | Node (x, l, r) -> inOrder l @ x :: inOrder r
        
    let binSort list = List.foldBack insert list Leaf |> inOrder