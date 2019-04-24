module Scrabble.ScrabbleAI
open ScrabbleUtil
open Scrabble
open CalculatePoints

    //TODO instantiate dict only once - now it happens each time decidemove is called.
    let englishAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let readLines filePath = System.IO.File.ReadLines filePath
    
    /// Create a dictionary from the given one-word-per-line file.
    let fromFile path = 
        let ed = Dictionary.empty englishAlphabet
        Seq.foldBack Dictionary.insert (readLines path) ed
    
    let decideMove (board:board) pieces (st:state) : move =
        let dic = fromFile "./EnglishDictionary.txt"
        let moves =
            match Map.tryFind (board.center) st.lettersPlaced with
            | None -> MoveGen.genStartingMoves board pieces st dic
            //OUTDATED
            | Some(_) -> MoveGen.genMoves board pieces st
        
        let withPoints = moves |> List.map (fun move -> prependCalcedPoints board move)
        let (pts,maxMove) = List.maxBy (fun (pts,move) -> pts) withPoints
        
        //TODO What if several moves with same point value?
        printfn "Max move: %d -> %A" pts maxMove
        maxMove
        
        // GOAL
        // Setup parsing |> movegen |> points |> max here
        // Possibly use async task handling. 