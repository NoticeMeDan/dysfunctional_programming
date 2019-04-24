namespace Scrabble

module GameState =

    let mkState lp h pieces= { lettersPlaced = lp; hand = h; piecesStore = pieces }
    
    let newState hand = mkState Map.empty hand
    
    let lettersPlaced st                 = st.lettersPlaced
    let hand st                          = st.hand
    let overwriteHand (st:state) newHand = mkState st.lettersPlaced newHand st.piecesStore
    
    let overwriteLettersPlaced (st:state) newLettersPlace = mkState newLettersPlace st.hand st.piecesStore
    
    /// Add placed pieces to the local board state and return the updated state
    let addPlacedPiecesToBoard (pcs:placedPiece list) (st:state) =
        let lettersPlaced' = List.fold (fun lettersPlaced (coord, (_, piece)) -> Map.add coord piece lettersPlaced) st.lettersPlaced pcs
        overwriteLettersPlaced st lettersPlaced'
    
    /// Add pieces to hand and return the updated state
    let addPiecesToHand (pcs:(uint32*uint32) list) (st:state) =
        let hand' = List.fold (fun acc (pid, x) -> MultiSet.add pid x acc) st.hand pcs
        overwriteHand st hand'
        
    /// Remove pieces from hand, given a list of moves played, and return the updated state
    let removePiecesFromHand (usedPiecesList:placedPiece list) st =
        let hand' = List.fold (fun acc (_, (pid, _)) -> MultiSet.removeSingle pid acc) st.hand usedPiecesList
        overwriteHand st hand'