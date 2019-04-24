module Scrabble.MoveGen

open MultiSet
open Scrabble
open ScrabbleUtil

let handIncludingPieces pieces state = MultiSet.map (fun id -> (id, Map.find id pieces)) state.hand

/// The edge case of the beginning move
let genStartingMoves (board:board) (pieces:Map<uint32, piece>) (state:state) dict =
    // The "hand" property of state is only a multiset of id's, we also need letters/pts
    let hand = handIncludingPieces pieces state
    
    // Recursive search tree, collecting all valid moves
    let rec aux stepX stepY searchTerm (coord:coord) (hand:MultiSet<(uint32*piece)>) placedSoFar (validMoves:move list) =
        match MultiSet.toList hand with
        | [] -> validMoves
        | pieces ->
            List.fold (
                fun validMoves (id, piece) ->
                //Might be a wildCard, i.e. a set containg whole alphabet. Otherwise singleton set with one (letter,pts) tuple, thus only folding once
                Set.fold (fun validMoves (letter,pts) -> 
                    let coord' = (coord |> fst |> (+) stepX,
                                  coord |> snd |> (+) stepY)
                    let hand' = (MultiSet.removeSingle (id, piece) hand)
                    let searchTerm' =
                        match stepX + stepY with
                        | -1 -> string letter + searchTerm
                        | 1 -> searchTerm + string letter
                        | _ -> failwith "invalid step %A" (stepX,stepY)
                    let placedSoFar' =
                        let placement = (coord, (id, (letter, pts)))
                        match stepX + stepY with
                        | -1 -> placement::placedSoFar
                        | 1 -> placedSoFar@[placement]
                        | _ -> failwith "invalid step %A" (stepX,stepY)
                    let nextAux = aux stepX stepY searchTerm' coord' hand' placedSoFar'
                    match Dictionary.lookup searchTerm' dict with
                    | true ->
                         //the pieces placed so far is a valid move, append it to the collection
                         nextAux (placedSoFar'::validMoves)
                    | false ->
                        //not a valid move, so keep recursing with unchanged collection
                        nextAux validMoves
                ) validMoves piece
            ) validMoves pieces
        
    let rowGen = aux 1 0 ""
    let rowGenRev = aux -1 0 ""
    let colGen = aux 0 1 ""
    let colGenRev = aux 0 -1 ""
    
    (colGen board.center hand [] [])@
    (rowGen board.center hand [] [])@
    (rowGenRev board.center hand [] [])@
    (colGenRev board.center hand [] [])
    
/// Entrypoint for generating moves
/// OUTDATED
let genMoves (board:board) (pieces:Map<uint32, piece>) (state:state) : move list =
         
    []

/// Given a single subproblem, finds several valid solutions/moves that satisfy the constraints of the problem
let genSubProblemSolution (problem:subProblem) : solution list = []