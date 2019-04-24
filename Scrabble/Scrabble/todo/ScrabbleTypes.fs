namespace Scrabble
open ScrabbleUtil

[<AutoOpen>]
module Types =
    ///Note that the "piece" type defined in ScrabbleUtil is Set(char * int), allowing wild cards and so on.
    /// This type is used "after" choosing the letter of the wildcard
    type placedPiece = (coord * (uint32 * (char * int)))
    
    ///Existing on the boord
    type existingPiece = coord * (char * int)
    
    /// A move consists of placing several pieces at coordinates
    /// This is the format accepted by the ScrabbleServer
    type move = placedPiece list
    
    type tileConstraint =
        /// No constraint at coordinate. Any "from hand"
        | Any of coord
        /// The tile is occupied by an existing piece
        | OccupiedBy of coord * (char * int)
        /// The piece placed at this tile is placed in front of an existing word in the crossing direction,
        /// and the combination of these two must form another valid word
        | MustEndWith of coord * existingPiece list
        /// The piece placed at this tile is placed after an existing word in the crossing direction,
        /// and the combination of these two must form another valid word
        | MustBeginWith of existingPiece list * coord
        /// The piece placed at this tile is placed between to existing words in the crossing direction,
        /// and the combination of these three must form a third valid word
        | MustJoin of existingPiece list * coord * existingPiece list
        
    /// A scrabble sub problem is limited to expressing the constraints of creating a valid "word",
    /// starting by placing pieces from a coordinate, given by the head of the tileConstraints-list
    /// and in a direction implicit from the coordinates of the other elements in the list.
    type subProblem = {
        /// At least this many pieces must be played in order for the word created to reach another existing piece on the board.
        /// In the most basic cases, where the sub problem starts next to an existing piece, this value is simply 1
        minNumPieces : int
        /// Solutions must begin with this ordered list of existing pieces
        prefix : existingPiece list;
        /// Solutions to this problem must satisify these constraints in the order given,
        /// i.e. begin by satisfying the first constraint, then the first AND second constraints an so on
        tileConstraints : tileConstraint list
        /// Solutions placing pieces in ALL available slots must end with this ordered list of existing pieces
        suffix : existingPiece list;
    }
    
    /// The result of a generated moves, which includes the necessary coordinate/piece data for calculating the accumulated score of the move
    type scoringPiece =
        /// An existing piece on the board
        | Old of coord * (char * int)
        /// A piece played this turn.
        /// The uint32 is the piece's id, which is required by the server when you send a move
        | New of coord * (uint32 * (char * int))
        /// A piece played including the information of a crossing word at this coordinate.
        /// THE NEW PIECE IS NOT INCLUDED IN THE LIST
        /// The uint32 is the piece's id, which is required by the server when you send a move
        | NewPlus of coord * (uint32 * (char * int)) * existingPiece list
    
    /// The solution to a sub problem. The "move" to be played, i.e. sent to the server, is implicit from the New/NewPlus values in the list
    type solution =
        scoringPiece list
        
    type state = {
        /// Maps board coordinates to pieces
        lettersPlaced : Map<ScrabbleUtil.coord, char * int>
        
        /// Pieces on your hand, NB: only identifiers. Look up the actual piece in the pieceStore
        hand          : MultiSet.MultiSet<uint32>
        
        /// A map from piece identifier to actual (char * int) piece
        piecesStore   : Map<uint32, piece>
    }