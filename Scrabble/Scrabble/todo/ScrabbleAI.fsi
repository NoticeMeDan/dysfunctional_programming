module Scrabble.ScrabbleAI

/// Given a game board, the id->piece map and the current game state, decides the "best" move,
/// that is, a list of piece instatiations
val decideMove : ScrabbleUtil.board -> Map<uint32,ScrabbleUtil.piece> -> state ->move