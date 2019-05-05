open System.IO

open ScrabbleServer
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.Net.Sockets
open MultiSet
open Dictionary

// From Jesper.
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

 // From Jesper.
 module Print =
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
        hand          : MultiSet<uint32>
        pieces        : Map<uint32, piece>
    }
    
    // Denotes a piece that *has* been placed on the board
    type placedPiece = (coord * (uint32 * (char * int)))
    
    type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>
    
    let mkState lp hand pieces = { lettersPlaced = lp; hand = hand; pieces = pieces }
    let newState hand = mkState Map.empty hand
    let updateHand state newHand = mkState state.lettersPlaced newHand state.pieces
    
    let addPiecesToBoard (pieces: placedPiece list) (state: state) =
        let updatedLettersPlaced =
            List.fold (fun lettersPlaced (coord, (_, piece)) ->
                Map.add coord piece lettersPlaced) state.lettersPlaced pieces
        mkState updatedLettersPlaced state.hand state.pieces
    
    let addPiecesToHand (pieces: (uint32 * uint32) list) (state: state) =
        updateHand state (
            List.fold (fun acc (id, x) -> MultiSet.add id x acc) state.hand pieces
        )
        
    let removeSwappedPiecesFromhand (pieces: (uint32 * uint32) list) (state: state) =
        updateHand state (
            List.fold (fun acc (id, amount) -> MultiSet.remove id amount acc) state.hand pieces
        )
        
    let removePlayedPiecesFromHand (usedPiecesList: placedPiece list) state =
        updateHand state (
            List.fold (fun acc (_, (id, _)) -> MultiSet.removeSingle id acc) state.hand usedPiecesList
        )

let rec createAnagram list =
    let lengthOfList = List.length list-1
    let array = [0 .. (lengthOfList - 1)]
    
    let rec anagram wordArray index map =
        match List.length wordArray < lengthOfList with
        | false -> []
        | true ->
            let nextWord = wordArray @ [list.[index]]
            let newMap = map |> Map.add index index 
            
            array |> List.fold (fun acc value ->
                match Map.tryFind value newMap with
                | None -> (anagram nextWord value newMap) @ acc
                | Some _ -> acc
                ) [nextWord]

    array |> List.fold (fun acc value -> (anagram [] value Map.empty) @ acc) []
    
let charsToString (chars: char list) = System.String.Concat(Array.ofList(chars))

let convertSetToList char =
    char |> Set.map (fun (c, _) -> c) |> Set.toArray
    
let rec setCharIntListToCharList list =
    match list with
    | [] -> []
    | (char : Set<char*int>) :: rest ->
        let list = char |> convertSetToList
        [list.[0]] @ (setCharIntListToCharList rest)

let convertToListOfStrings list =
    list |> List.map (fun x -> x |> setCharIntListToCharList |> charsToString)

let rec calculatePointsOfWord word =
    match word with 
    | [] -> 0
    | (_, letters) :: tail -> (snd (letters |> Set.toList).Head) + (calculatePointsOfWord tail) 

let makeMove word startPosition (toCoord: coord) =
    word
    |> List.map (fun (num ,x) ->
        (uint32 num, (Set.toList x).Head))
    |> List.fold (fun ((x, y), char) value ->
          ((x + fst toCoord, y + snd toCoord), ((x,y), value) :: char))
          (startPosition, [])
    |> fun (_, x) -> x
    |> SMPlay

let mapPiecesToIndexes pieces hand =
    hand
    |> MultiSet.fold
        (fun acc i numAvailable -> [1u .. numAvailable] |> List.fold (fun innerAcc _ -> innerAcc @ [i, Map.find i pieces]) acc) []
    |> List.fold
      (fun acc (i, set) ->
        let rec innerFunc result rest =
            match rest with
            | [] -> result
            | (character, points) :: tail -> innerFunc ((i, character, points ):: result) tail
        (innerFunc [] (Set.toList set)) @ acc
        ) []
    |> List.sortBy (fun (_, _, points) -> points)
    |> List.map (fun (i, character, _) -> i, character)
    |> List.fold (fun acc (i, character) ->
        let found = Map.tryFind character acc
        if found = None then
            Map.add character [i] acc
        else
            Map.add character (i::found.Value) acc) Map.empty

let piecesMapToList pieces =
    MultiSet.fold
        (fun acc index ammountAvailable ->
            [1u .. ammountAvailable]
            |> List.fold (fun acc2 _ ->
                acc2 @ [Map.find index pieces]) acc)
        []
        
let createAnagramFromHand hand pieces = 
    hand
    |> piecesMapToList pieces
    |> createAnagram
    |> convertToListOfStrings

let createAnagramFromStartChar hand pieces startCharList length= 
    hand
    |> piecesMapToList pieces
    |> createAnagram
    |> convertToListOfStrings
    |> List.filter (fun string -> length >= string.Length)
    |> List.map (fun string -> (startCharList |> charsToString) + string)

let convertStringToPiece (mapCharToIndexes: Map<'a, 'b list>) pieces word =
    List.fold (fun a c ->
                  let indexes = Map.find c mapCharToIndexes
                  (fst a @ [(indexes.Head, Map.find indexes.Head pieces)],
                   mapCharToIndexes |> Map.add c indexes.Tail))
        ([], mapCharToIndexes) (Seq.toList word)
        
let convertWordsToPieces words mapCharToIndexes pieces = 
    List.map (fun word -> word |> convertStringToPiece mapCharToIndexes pieces) words
    |> List.map (fun x -> fst x)

let findLegalWords words dictionary = 
    words
    |> List.filter (fun x -> Dictionary.lookup x dictionary)
    |> List.distinct

let playFirstMove center (state : State.state) dict =
    let legalWords = findLegalWords (createAnagramFromHand state.hand state.pieces) dict
    let pieces = convertWordsToPieces legalWords (mapPiecesToIndexes state.pieces state.hand) state.pieces
                |> List.sortByDescending (fun word -> calculatePointsOfWord word)
    
    match pieces with
    | [] -> SMPass
    | word::_ -> makeMove word center (1, 0)

let findBestWordForRow pieces hand charList length (dict: Dictionary) = 
    let words = createAnagramFromStartChar hand pieces charList length
    let legalWords =
        findLegalWords words dict
        |> List.map (fun string -> string.Remove (0, (List.length charList)))

    convertWordsToPieces legalWords (mapPiecesToIndexes pieces hand) pieces
    |> List.map (fun x -> async { return calculatePointsOfWord x, x })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.toList
    |> List.sortByDescending (fun (sum, _) -> sum)

let tileContent coord placed board =
    let tile = Map.tryFind coord placed, ScrabbleUtil.Board.tiles board coord
    match tile with
    | None, Some (char, _) -> char
    | Some (char, _), _    -> char
    | _, None -> ' '

let isTileEmpty (coord:coord) placed board boardRadius =
    let char = tileContent coord placed board
    let tileEmpty = (System.Char.IsLower char) || (char = ' ')
    let isOnBoard = (fst coord) < boardRadius+1 && (snd coord) < boardRadius+1
    (tileEmpty && isOnBoard) 

let emptyPlacesInDirection (coord:coord) (state:State.state) board moveX moveY boardRadius =
    let placedPieces = state.lettersPlaced
    let handSize = state.lettersPlaced |> Map.toList |> List.length
    
    let newCoordinate coord = ((fst coord) + moveX, (snd coord) + moveY)
    
    let neighbourCheck coord =
        let neighbourCheck1 = isTileEmpty (fst coord + moveY, snd coord + moveX) placedPieces board boardRadius
        let neighbourCheck2 = isTileEmpty (fst coord - moveY, snd coord - moveX) placedPieces board boardRadius
        neighbourCheck1 && neighbourCheck2
    
    let rec numberOfEmpty coord acc =
        if (acc < handSize)
        then
            let newCoords = newCoordinate coord
            let neighbourAlright = neighbourCheck newCoords
            match isTileEmpty newCoords placedPieces board boardRadius, neighbourAlright with
            | true, true ->  (numberOfEmpty newCoords (acc + 1))
            | false, _ -> acc-1
            | true, false -> acc
        else acc
    numberOfEmpty coord 0

let wordAdjacentToTile coord placed board moveX moveY radius =
    let rec inner coord value =
        match isTileEmpty coord placed board radius with
        | true  -> value
        | _     ->
            let newCoords = ((fst coord) + moveX, (snd coord) + moveY)
            inner newCoords (tileContent coord placed board::value)
    inner coord []

let findOccupiedTiles (state: State.state) : coord list =
        let letters = Map.toList state.lettersPlaced;
        let rec tileLocationsRec tail = 
            match tail with
            | [] -> []
            | (x : coord, _) :: rest -> x :: tileLocationsRec(rest)
        tileLocationsRec (letters)   

let findRowWithMostEmptyTiles board (state : State.state) radius =
    let mapEmptyPlaces list =
        List.map (fun coord ->
            let xs = emptyPlacesInDirection coord state board 1 0 radius
            let ys = emptyPlacesInDirection coord state board 0 1 radius
            (coord, xs, ys)) list
    
    let mapNeighbouringWords list =
        List.map (fun (coord, placesX, placesY) ->
            ((wordAdjacentToTile coord state.lettersPlaced board -1 0 radius,
              wordAdjacentToTile coord state.lettersPlaced board 0 -1 radius),
             (coord, placesX, placesY))) list
    
    let update list =
        List.fold (fun map ((stringX, stringY), ((x,y), spaceX, spaceY)) ->
                let append string xs ys acc = Map.add string ((x,y), xs, ys) acc   
                let updateMap string horizontalSpace verticalSpace map =
                    match (horizontalSpace,verticalSpace) with
                    | (-1,-1) -> map
                    | _ ->
                        let maxSpace = max spaceX spaceY
                        match Map.tryFind string map with
                        | None -> map |> append string horizontalSpace verticalSpace
                        | Some (_, xs, ys) -> 
                            if (max xs ys) < maxSpace
                            then map |> append string horizontalSpace verticalSpace
                            else map
            
                if stringX = stringY
                then  map |> updateMap stringX spaceX spaceY
                else  map |> updateMap stringX spaceX -1 |> updateMap stringY -1 spaceY  
            )
            Map.empty list
            
    let findRowWithMostEmptyTiles =
        findOccupiedTiles state
        |> mapEmptyPlaces
        |> mapNeighbouringWords
        |> update
    
    findRowWithMostEmptyTiles

let findBestMove row pieces (state : State.state) (dict:Dictionary) =
    let findBestWords list =
        List.map (fun (charLst, ((x,y),  placesX, placesY)) -> 
            let words = findBestWordForRow pieces state.hand charLst (max placesX placesY) dict
            let fstWord =
                match words with
                | [] -> None
                | first :: _ -> Some first 
            (charLst, fstWord)
            ) list
        
    let findPlayableMoves =
        row
        |> Map.toList
        |> findBestWords
        |> List.filter (fun ( _ , word) -> match word with | None -> false | Some _ -> true)
        |> List.map (fun (charLst, word) -> (charLst, word.Value))
        |> List.sortBy (fun (_, (sum, _)) -> sum)
        
    match findPlayableMoves with
    | [] when Seq.length state.lettersPlaced < 20 -> SMChange [(MultiSet.toList state.hand).Head; ((MultiSet.toList state.hand).Tail).Head ]
    | [] -> SMForfeit
    | (charLst, (_, word)) :: _ -> 
        let ((x,y), placesX, placesY) = Map.find charLst row
        
        let distanceX, distanceY =
            if placesX >= placesY then (1,0)
            else (0,1)
        
        let x = distanceX + x
        let y = distanceY + y
        let coord = (x,y)
        
        makeMove word coord (distanceX, distanceY)
    
let playBestMove board (state : State.state) radius (dict:Dictionary) =
    let pieces = state.pieces
    let bestRow = findRowWithMostEmptyTiles board state radius
    findBestMove bestRow pieces state dict

let AIPlay board pieces (state : State.state) radius (dict:Dictionary)=
    match Map.tryFind (board.center) state.lettersPlaced, ScrabbleUtil.Board.tiles board (board.center) with
    | None, Some (' ', _) ->      
        playFirstMove (board.center) state dict
    | _, _    ->                                                   
        playBestMove board state radius dict

let createDictionary words =
    let englishAlfabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    List.fold (fun acc s -> Dictionary.insert s acc) (Dictionary.empty englishAlfabet) words

let playScrabble cstream board pieces (state : State.state) words =
    let dict = createDictionary words
        
    let rec gameLoop (state : State.state) =
        Print.printBoard board 8 (state.lettersPlaced)

        let hasWildcard = MultiSet.contains 0u state.hand
        
        let move =
            if hasWildcard
            then
                // Swap the amount of wildcards on hand
                SMChange (List.replicate (int((MultiSet.numItems 0u state.hand))) 0u) 
            else
                AIPlay board pieces state 8 dict
        
        if (move.GetType() = SMForfeit.GetType()) then printfn "AI: No move available."
        printfn "Trying to play: %A" move
        send cstream (move)
        let msg = recv cstream
        match msg with
        | RCM (CMPlaySuccess(moves, points, newPieces)) ->
            printfn "You moved: %A" moves
            printfn "Points for move: %A" points
            printfn "New pieces %A" newPieces
            
            let newState = state |> (State.addPiecesToBoard moves
                         >> State.removePlayedPiecesFromHand moves
                         >> State.addPiecesToHand newPieces) 
            gameLoop newState
        | RCM (CMChangeSuccess (newPieces)) ->
            printfn "You swapped a piece(s)."
            printfn "New piece(s): %A" newPieces
            
            let firstTwoPieces = ((MultiSet.toList state.hand).Head, ((MultiSet.toList state.hand).Tail).Head)
            let swapped = (Seq.toList (move.ToString())).Item(10)
            // Remove wildcards, then add newPiecesgl
            let newState =
                if (swapped = '0')
                then state |> (State.removeSwappedPiecesFromhand [(0u, MultiSet.numItems 0u state.hand)]
                                     >> State.addPiecesToHand newPieces )
                else state |> (State.removeSwappedPiecesFromhand [(fst firstTwoPieces, 1u); (snd firstTwoPieces, 1u)]
                                     >> State.addPiecesToHand newPieces )
                
            gameLoop newState
        | RCM (CMPlayed (pid, moves, _)) ->
            printfn "Player %A, played:\n %A" pid moves
            let newState = state |> State.addPiecesToBoard moves
            gameLoop newState
        | RCM (CMPlayFailed (_, _)) ->
            let newState = state
            gameLoop newState
        | RCM (CMGameOver _) -> ()
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RErr err -> printfn "Server Error:\n%A" err; gameLoop state
        | RGPE err -> printfn "Gameplay Error:\n%A" err; gameLoop state

    gameLoop state

// From Jesper
let setupGame cstream board words =
    let rec aux () =
        match ServerCommunication.recv cstream with
        | RCM (CMPlayerJoined name) ->
            printfn "Player %s joined" name
            aux ()
        | RCM (CMGameStarted (_, hand, _, pieces, _)) as msg ->
            printfn "Game started %A" msg
            let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
            playScrabble cstream board pieces (State.newState handSet pieces) words
        | msg -> failwith (sprintf "Game initialisation failed. Unexpected message %A" msg)
    aux ()

// From Jesper
let joinGame port gameId password playerName =
    async {
        let client = new TcpClient(sprintf "%A" (localIP ()), port)
        use cstream = client.GetStream()
        send cstream (SMJoinGame (gameId, password, playerName))

        match ServerCommunication.recv cstream with
            | RCM (CMJoinSuccess(board, _, _, words, _, _)) -> 
                setupGame cstream board words
            | msg -> failwith (sprintf "Error joining game%A" msg)
    }

// From Jesper
let startGame port numberOfPlayers = 
    async {
        let client = new TcpClient(sprintf "%A" (localIP ()), port)
        let path = "../../../EnglishDictionary.txt"
        let cstream = client.GetStream()
        let board = StandardBoard.mkStandardBoard ()
        let words = File.ReadLines path |> Seq.toList
        let pieces = English.pieces()
        let seed = None
        let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let timeout = None
        let sizeOfHand = 7u

        send cstream (SMStartGame (numberOfPlayers, "game", "password", "NoticeMeDan", seed, board, pieces,
                                    sizeOfHand, alphabet, words, timeout))

        let gameId =
            match ServerCommunication.recv cstream with
            | RCM (CMGameInit gameId) -> gameId
            | msg -> failwith (sprintf "Error initialising game, server sent other message than CMGameInit (should not happen)\n%A" msg)
            
        do! (async { setupGame cstream board words } ::
             [for i in 2u..numberOfPlayers do yield joinGame port gameId "password" ("Player" + (string i))] |>
             Async.Parallel |> Async.Ignore)
    }

// From Jesper
[<EntryPoint>]
let main _ =
    [Comm.startServer 13000; startGame 13000 1u] |>
    Async.Parallel |>
    Async.RunSynchronously |> ignore
    0 // return an integer exit code