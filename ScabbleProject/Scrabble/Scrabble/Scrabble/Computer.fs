module Scrabble.Computer
    let calculateMove board pieces state =
        // save current word
        // Recursively:
            // choose direction
            // check if we can make a legal word on next tile
            // if we can, create move, else change direction
        []
    
    let makeMove (board:ScrabbleUtil.board) pieces (state:State.state) coord =
        // let dictionary = get file from path
        match Map.tryFind(board.center) state.lettersPlaced with
            | None -> printf "no letter on center"
            | Some(_) -> calculateMove board pieces state
        