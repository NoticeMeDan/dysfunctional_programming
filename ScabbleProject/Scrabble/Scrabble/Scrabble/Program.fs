open ScrabbleServer
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =
    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> printfn "%d -> (%A, %d)" x (Map.find x pieces) i) ()

    let printBoard board radius placed =

        let c = ScrabbleUtil.Board.center board

        let minX = fst c - radius
        let maxX = fst c + radius
        let minY = snd c - radius
        let maxY = snd c + radius

        for y in [minY..maxY] do
            for x in [minX..maxX] do
                match Map.tryFind (x, y) placed, ScrabbleUtil.Board.tiles board (x, y) with
                | None, Some (c, _) -> printf "%c " c
                | Some (c, _), _    -> printf "%c " c
                | _, None -> printf "# "
            printf "\n"

module State = 
    type state = {
        lettersPlaced : Map<ScrabbleUtil.coord, char * int>
        hand          : MultiSet.MultiSet<uint32>
        pieces        : Map<uint32, piece>
    }

    let makeState lp h pieces = { lettersPlaced = lp; hand = h; pieces = pieces }

    let newState hand = makeState Map.empty hand

    let lettersPlaced state = state.lettersPlaced
    let hand state          = state.hand
    
    // Overwrite playerhand with new state hand
    let overwriteHand state newHand = makeState state.lettersPlaced newHand state.pieces
    
    let overwriteLettersPlaced state newLettersPlace = makeState newLettersPlace state.hand state.pieces
    
    /// Add placed pieces to the local board state and return the updated state
    let addPlacedPiecesToBoard (pcs:piece list) (st:state) =
        let lettersPlaced' = List.fold (fun lettersPlaced (coord, (_, piece)) -> Map.add coord piece lettersPlaced) st.lettersPlaced pcs
        overwriteLettersPlaced st lettersPlaced'
    
    /// Add pieces to hand and return the updated state
    let addPiecesToHand (pcs:(uint32*uint32) list) (st:state) =
        let hand' = List.fold (fun acc (pid, x) -> MultiSet.add pid x acc) st.hand pcs
        overwriteHand st hand'
        
    /// Remove pieces from hand, given a list of moves played, and return the updated state
    //     let removePiecesFromHand (usedPiecesList:piece list) st =
    //     let hand' = List.fold (fun acc (_, (pid, _)) -> MultiSet.removeSingle pid acc) st.hand usedPiecesList
    //     overwriteHand st hand'
    
let recv play st msg =
    match msg with
    | RCM (CMPlaySuccess(ms, points, newPieces)) ->
        printfn "Successful play. Points earned: %d" points;
        printfn "Piece: %A, added to your hand" newPieces;
        // TODO: update state with:
        // Add piece to board.
        // Remove piece from hand.
        // Add new piece to hand.
        (* Successful play by you. Update your state *)
        let st' = st // This state needs to be updated
        play st'
    | RCM (CMPlayed (pid, ms, points)) ->
        (* Successful play by other player. Update your state *)
        printfn "Player: %d, played %A and got %d points" pid ms points;
        // TODO: update state with new pieces on the board
        let st' = st // This state needs to be updated
        play st'
    | RCM (CMPlayFailed (pid, ms)) ->
        (* Failed play. Update your state *)
        printfn "Failed to play: %A" ms
        let st' = st // This state needs to be updated
        play st'
    | RCM (CMGameOver _) -> ()
    | RCM a -> failwith (sprintf "not implmented: %A" a)
    | RErr err -> printfn "Server Error:\n%A" err; play st
    | RGPE err -> printfn "Gameplay Error:\n%A" err; play st

let playGame send board pieces st =

    let rec aux st =
        Print.printBoard board 8 (State.lettersPlaced st)
        printfn "\n\n"
        Print.printHand pieces (State.hand st)

        printfn "Input move (format '(<x-coordinate><y-coordinate> <piece id><character><point-value> )*', note the absence of state between the last inputs)"
        let input =  System.Console.ReadLine()
        // TODO: find a possible move
        let move = RegEx.parseMove input

        send (recv aux st) (SMPlay move)

    aux st

let startGame send (msg : Response) = 
    match msg with
    | RCM (CMGameStarted (board, pieces, playerNumber, hand, playerList)) ->
        let hand' = List.fold (fun acc (v, x) -> MultiSet.add v x acc) MultiSet.empty hand
        playGame send board pieces (State.newState hand' pieces)
    | _ -> failwith "No game has been started yet"
     
[<EntryPoint>]
let main argv =
    let send = Comm.connect ()
    send (startGame send) (SMStartGame(1u, "My game", "", "My name"))
    0 // return an integer exit code