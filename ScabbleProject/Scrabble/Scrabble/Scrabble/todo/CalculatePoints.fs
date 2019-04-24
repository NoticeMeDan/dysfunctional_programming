namespace Scrabble

module CalculatePoints =
    open ScrabbleUtil
    /// Map<priority, index -> wordArray -> points so far -> points total
    type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>
    

    /// Calculates the point value of placing the "letters" on top of underlying "tiles"
    /// The two inputs are expected to have the same length and correct ordering
    let ptsOfSingleWord (tiles:tile list) (letters:(char*int)[]) =
        //TODO Input validation, i.e. length matches, nonempty
        
        //recursively apply letters to the tile point functions and yield funcs as seqence
        let rec applyLetters seq =
            function
            | letterIndex when letterIndex < tiles.Length ->
                let (_, tileMap) = tiles.[letterIndex]
                
                //functions (priority,func) now simply takes the points so far and adds points from "this" tile to the total
                //see documentation to understand how tile functions work
                let funcSeq = Map.map (fun k_priorityKey v_pointFunction -> v_pointFunction (uint32 letterIndex) letters) tileMap
                              |> Map.toSeq
                applyLetters (Seq.append seq funcSeq) (letterIndex+1) 
            | _ -> seq
        
        let startIndex = 0
        applyLetters Seq.empty startIndex
       |> Seq.sortBy (fun (priority, func) -> priority)
       |> Seq.fold (fun pts (_,tile) -> tile pts) 0
    
    let getTile board coord =
        match board.tiles coord with
        | Some(tile) -> tile
        | None -> failwith "bad move"
    
    /// Calculate points of valid move with crossing words
    /// Uses the output format of generated valid solution as input, to accomodate simple accumulation calcaluation of score
    let totalPoints board (pieces:scoringPiece list) =
        // TODO Refactor to use continuations? @ performance problematic?
        (* From Ass 5
        let rec bigListK c =
            function
            | 0 -> c []
            | n -> bigListK (fun res -> 1 :: c res) (n - 1)
        *)
        let rec aux (accPts:int) pieces mainWordPieces mainWordTiles =
            match pieces with
            //end of main word: add accumulated crossing word scores to the score of the main word.
            | [] -> accPts + (ptsOfSingleWord mainWordTiles (List.toArray mainWordPieces))
            
            | piece::rest ->
                match piece with
                | Old(coord, (letter, pt)) ->
                    let mainWordPieces' = (mainWordPieces@[(letter,pt)])
                    let mainWordTiles' = (mainWordTiles@[board.usedTile])
                    aux accPts rest mainWordPieces' mainWordTiles'
                | New(coord, (id, (letter,pt))) ->
                    let mainWordPieces' = (mainWordPieces@[(letter,pt)])
                    let mainWordTiles' = (mainWordTiles@[getTile board coord])
                    aux accPts rest mainWordPieces' mainWordTiles'
                | NewPlus(coord, (id, (letter, pt)), lst) ->
                    //setup for calculation of crossing word. Add the tile beneath the newly placed piece to a list of "used" tiles
                    let unusedTileWithUsedTiles = (getTile board coord)::(List.init lst.Length (fun i -> board.usedTile))
                    //place newly placed letter in front of the existing old letters.
                    let newLetterWithOldLetters = (letter,pt)::(List.map (fun (coord,(letter,pt)) -> (letter,pt)) lst) |> List.toArray
                    //hopefully this creates a list + array of equal length :-|
                    
                    let crossingWordPts = ptsOfSingleWord unusedTileWithUsedTiles newLetterWithOldLetters
                    let accPts' = accPts + crossingWordPts
                    let mainWordPieces' = (mainWordPieces@[(letter,pt)])
                    let mainWordTiles' = (mainWordTiles@[getTile board coord])                    
                    
                    aux accPts' rest mainWordPieces' mainWordTiles'
                    
//        let bonus = if (List.where (fun sb -> sb = New(_,_)) pieces).Length = 7 then 50 else 0 TODO fix bonus when using whole hand
        aux 0 pieces [] []
    
    /// Transform a move into a (pts,move) tuple
    /// Outdated
    let prependCalcedPoints board = fun move ->
        let tiles = move |> List.map (fun (coord, (id, (letter, pt))) ->
            match board.tiles coord with
            | Some (tile:tile )-> tile
            | None -> failwith ("Invalid placement of " + string (coord,letter))
            )
        let letters = move |> List.toArray |> Array.map (fun (coord, (id, (letter, pt))) -> (letter, pt))
        let points = ptsOfSingleWord tiles letters
        printfn "Word: %A - Points: %d" letters points
        (points , move)