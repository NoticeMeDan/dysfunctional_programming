open ScrabbleServer
open ScrabbleUtil.ServerCommunication
open ScrabbleUtil

/// uint32: pieceId

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

let recv play (st:state) msg =
    match msg with
    | RCM (CMPlaySuccess(moves, points, newPieces)) ->
        printfn "Points scored: %d" points;
        printfn "New pieces: %A" newPieces
        
        let st' = st |> (addPlacedPiecesToBoard moves
                        >> removePiecesFromHand moves
                        >> addPiecesToHand newPieces)
        play st'
    | RCM (CMPlayed (pid, ms, points)) ->
        (* Successful play by other player. Update your state *)
        printfn "Player %d scored %d points" pid points
        let st' = st |> (addPlacedPiecesToBoard ms)
        play st'
    | RCM (CMPlayFailed (pid, ms)) ->
        (* Failed play. Update your state *)
        printfn "Failed play: %A" ms
        let st' = st // This state needs to be updated
        play st'
    | RCM (CMGameOver _) -> ()
    | RCM a -> failwith (sprintf "not implmented: %A" a)
    | RErr err -> printfn "Server Error:\n%A" err; play st
    | RGPE err -> printfn "Gameplay Error:\n%A" err; play st
    
let playGame send board pieces st =

    let rec aux st =
        Print.printBoard board 8 (lettersPlaced st)
        printfn "\n\n"
        Print.printHand pieces (hand st)

        printfn "Type 'ai' or \nInput move (format '(<x-coord> <y-coord> <piece-key><character><point-value> )*')"
        let input =  System.Console.ReadLine()
        
        let move =
            match input with
            | "ai" -> ScrabbleAI.decideMove board pieces st
            | s -> RegEx.parseMove s

//        printfn "Your calculated points: %d" calculatePoints 
        send (recv aux st) (SMPlay move)

    aux st

let startGame send (msg : Response) = 
    match msg with
    | RCM (CMGameStarted (board, pieces, playerNumber, hand, playerList)) ->
        let hand' = List.fold (fun acc (v, x) -> MultiSet.add v x acc) MultiSet.empty hand
        playGame send board pieces (newState hand' pieces)
    | _ -> failwith "No game has been started yet"
    
[<EntryPoint>]
let main argv =
    let send = Comm.connect ()
    send (startGame send) (SMStartGame(1u, "My game", "", "My name"))
    0 // return an integer exit code