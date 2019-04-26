open System.IO

open ScrabbleServer
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.Net.Sockets

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
    
    type piecePlaced = (coord * (uint32 * (char * int)))
    
    type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>
    
    let singleLetterScore : tile = ('a', Map.add 0u (fun i cs p -> p + (cs.[int i] |> snd) * 10) Map.empty)
    let doubleLetterScore : tile = ('c', Map.add 0u (fun i cs p -> p + (cs.[int i] |> snd) * 2) Map.empty)
    let trippleLetterScore : tile = ('b', Map.add 0u (fun i cs p -> p + (cs.[int i] |> snd) * 3) Map.empty)
    
    let makeState lettersPlaced hand pieces = { lettersPlaced = lettersPlaced; hand = hand; pieces = pieces }

    let newState hand = makeState Map.empty hand

    let lettersPlaced state = state.lettersPlaced
    let hand state          = state.hand
    
    // Overwrite playerhand with new state hand
    let overwriteHand state newHand = makeState state.lettersPlaced newHand state.pieces
    
    let overwriteLettersPlaced state newLettersPlace = makeState newLettersPlace state.hand state.pieces
    
    /// Add placed pieces to the local board state and return the updated state
    let addPlacedPiecesToBoard (pcs:piecePlaced list) (st:state) =
        let lettersPlaced' =
            List.fold (fun lettersPlaced (coord, (_, piece)) ->
                Map.add coord piece lettersPlaced) st.lettersPlaced pcs
        overwriteLettersPlaced st lettersPlaced'
    
    /// Add pieces to hand and return the updated state
    let addPiecesToHand (pcs:(uint32*uint32) list) (st:state) =
        let hand' = List.fold (fun acc (pid, x) -> MultiSet.add pid x acc) st.hand pcs
        overwriteHand st hand'
        
    /// Remove pieces from hand, given a list of moves played, and return the updated state
    let removePiecesFromHand (usedPiecesList:piecePlaced list) st =
        let hand' = List.fold (fun acc (_, (pid, _)) -> MultiSet.removeSingle pid acc) st.hand usedPiecesList
        overwriteHand st hand'

module Computer =
    let calculateNextMove board pieces state = 
    // save current word
    // Recursively:
        // choose direction
        // check if we can make a legal word on next tile
        // if we can, create move, else change direction
        []
        
    let calculateFirstMove board pieces state =
        []

    let makeMove (board:ScrabbleUtil.board) pieces (state:State.state) coord = 
        // let dictionary = get file from path
        match Map.tryFind(board.center) state.lettersPlaced with
            | None -> calculateFirstMove board pieces state
            | Some(_) -> calculateNextMove board pieces state
            
let createDictionary words =
    let englishAlfabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let dict = List.fold (fun acc s -> Dictionary.insert s acc) (Dictionary.empty englishAlfabet) words
    
    dict

let playGame cstream board pieces (st : State.state) words =
    let dict = createDictionary words
    let lookup word =
        Dictionary.lookup word dict
        
    let rec aux (st : State.state) =
        Print.printBoard board 8 (State.lettersPlaced st)
        printfn "\n\n"
        Print.printHand pieces (State.hand st)

        printfn "Input move (format '(<x-coordinate><y-coordinate> <piece id><character><point-value> )*', note the absence of state between the last inputs)"
        let input =  System.Console.ReadLine()
        printf "Word: %A -> %A\n" input (lookup input)
        let move =
            match input with
            //| "" -> Computer.makeMove board pieces st 
            | s -> RegEx.parseMove input

        printfn "Trying to play: %A" move
        send cstream (SMPlay move)
        let msg = recv cstream
        match msg with
        | RCM (CMPlaySuccess(moves, points, newPieces)) ->
            (* Successful play by you. Update your state *)
            printfn "Success. You moved: %A" moves
            printfn "points: %A" points
            printfn "new pieces %A" newPieces
            
            let st' = st |> (State.addPlacedPiecesToBoard moves
                         >> State.removePiecesFromHand moves
                         >> State.addPiecesToHand newPieces) 
            aux st'
        | RCM (CMPlayed (pid, moves, points)) ->
            (* Successful play by other player. Update your state *)
            printfn "Player %A, played:\n %A" pid moves
            let st' = st |> State.addPlacedPiecesToBoard moves
            aux st'
        | RCM (CMPlayFailed (pid, ms)) ->
            (* Failed play. Update your state *)
            let st' = st // This state needs to be updated
            aux st'
        | RCM (CMGameOver _) -> ()
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RErr err -> printfn "Server Error:\n%A" err; aux st
        | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st

    aux st

let setupGame cstream board alphabet words handSize timeout =
    let rec aux () =
        match ServerCommunication.recv cstream with
        | RCM (CMPlayerJoined name) ->
            printfn "Player %s joined" name
            aux ()
        | RCM (CMGameStarted (playerNumber, hand, firstPlayer, pieces, players)) as msg ->
            printfn "Game started %A" msg
            let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
            playGame cstream board pieces (State.newState handSet pieces) words
        | msg -> failwith (sprintf "Game initialisation failed. Unexpected message %A" msg)
        
    aux ()

let joinGame port gameId password playerName =
    async {
        let client = new TcpClient(sprintf "%A" (localIP ()), port)
        use cstream = client.GetStream()
        send cstream (SMJoinGame (gameId, password, playerName))

        match ServerCommunication.recv cstream with
            | RCM (CMJoinSuccess(board, numberOfPlayers, alphabet, words, handSize, timeout)) -> 
                setupGame cstream board alphabet words handSize timeout 
            | msg -> failwith (sprintf "Error joining game%A" msg)
    }

(*let createDictionary filePath =
    let englishAlphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let readLines filePath = System.IO.File.ReadLines filePath
    let ed = Dictionary.empty englishAlphabet
    
    Seq.foldBack Dictionary.insert (readLines filePath) ed*)

let startGame port numberOfPlayers = 
    async {
        let client = new TcpClient(sprintf "%A" (localIP ()), port)
        let cstream = client.GetStream()
        let path = "../../../EnglishDictionary.txt"
        let words = File.ReadLines path |> Seq.toList
        let board = StandardBoard.mkStandardBoard ()
        let pieces = English.pieces()
        let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let handSize = 7u
        let timeout = None
        let seed = None

        send cstream (SMStartGame (numberOfPlayers, "My game", "password", "My name", seed, board, pieces,
                                    handSize, alphabet, words, timeout))

        let gameId =
            match ServerCommunication.recv cstream with
            | RCM (CMGameInit gameId) -> gameId
            | msg -> failwith (sprintf "Error initialising game, server sent other message than CMGameInit (should not happen)\n%A" msg)
            
        do! (async { setupGame cstream board alphabet words handSize timeout } ::
             [for i in 2u..numberOfPlayers do yield joinGame port gameId "password" ("Player" + (string i))] |>
             Async.Parallel |> Async.Ignore)
    }
    
[<EntryPoint>]
let main argv =
    [Comm.startServer 13000; startGame 13000 1u] |>
    Async.Parallel |>
    Async.RunSynchronously |> ignore
    0 // return an integer exit code

