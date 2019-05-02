open System.IO

open ScrabbleServer
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.Net.Sockets

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
    // Jesper
    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> printfn "%d -> (%A, %d)" x (Map.find x pieces) i) ()
    
    // Jesper
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
    
    let printPoints points names =
        points
        |> Map.toList
        |> List.iter(fun (k,v) -> printfn "player %A have points: %A" (Map.find k names) v )

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
    
    let makeState lettersPlaced hand pieces = { state.lettersPlaced = lettersPlaced; state.hand = hand; state.pieces = pieces }

    let newState hand = makeState Map.empty hand

    let lettersPlaced state = state.lettersPlaced
    let hand state          = state.hand
    
    let overwriteHand state newHand = makeState state.lettersPlaced newHand state.pieces
    
    let overwriteLettersPlaced state newLettersPlace = makeState newLettersPlace state.hand state.pieces
    
    // Add placed pieces to the local board state and return the updated state
    // TODO rest of the method
    let addPlacedPiecesToBoard (pieces:piecePlaced list) (state:state) =
        let updateLettersPlaced =
                List.fold (fun lettersPlaced (coord, (_, piece)) ->
                    Map.add coord piece lettersPlaced) state.lettersPlaced pieces
        overwriteLettersPlaced state updateLettersPlaced
    
    // Add pieces to hand and return the updated state
    // TODO rest of the method
    let addPiecesToHand (pcs:(uint32*uint32) list) (st:state) =
        let hand' = List.fold (fun acc (pid, x) -> MultiSet.add pid x acc) st.hand pcs
        overwriteHand st hand'
        
    // Remove pieces from hand, given a list of moves played, and return the updated state
    // TODO rest of the method
    let removePiecesFromHand (usedPiecesList:piecePlaced list) st =
        let hand' = List.fold (fun acc (_, (pid, _)) -> MultiSet.removeSingle pid acc) st.hand usedPiecesList
        overwriteHand st hand'

module CalculatePoints =
    let calculatePoints (tiles: tile list) (words: (char * int) array) =
        let pset =
            tiles |> List.fold
                         (fun acc tile -> Map.fold (fun mapAcc key value -> Set.add key mapAcc) acc (snd tile))
                         Set.empty<uint32>

        let get tile p pos =
           match Map.tryFind p (snd tile) with
           | None -> id
           | Some f -> f pos words

        let processor p = tiles |> List.fold (fun (f, i) t ->  ((get t p i) :: f, i + 1u)) ([], 0u) |> fst
        let fl = Set.foldBack (fun ks s -> (processor ks) @ s) pset []
        let compute = fl |> List.fold (>>) id

        compute 0

// List is : List<List<String>>
let rec findAnagrams list =
    let length = List.length list
    
    // Word is a List<String>
    // TODO rest of the method
    let rec combine wordList index map =
        if List.length wordList < length then
            // Next wordList is current wordlist, with the appended list[index]
            let nextWordList = wordList @ [list.[index]]
            let newMap = map |> Map.add index index 
            
            [0 .. (length-1)]
            |> List.fold (fun acc index ->
                match Map.tryFind index newMap with
                | None -> (combine nextWordList index newMap) @ acc
                | Some _ -> acc
                ) [nextWordList]
        else []
    
    // Combine for every index of the given list. 
    [0 .. (length-1)] |> List.fold (fun acc index -> (combine [] index Map.empty)@acc) []
    
let charactersToString (characters:char list) = List.foldBack (fun x acc -> x.ToString() + acc) characters ""

let rec convertToCharList list =
    // TODO rest of the method
    match list with
    | [] -> []
    | (char : Set<char*int>) :: xtt ->
        let x = char |> Set.map (fun (k, v) -> k) |> Set.toArray
        
        [x.[0]] @ (convertToCharList xtt) // returning the first. Change this to return all
        
let convertToListOfStrings (list : Set<char*int> list list) =
    list |> List.map (fun x -> x |> convertToCharList |> charactersToString)

let pointSumOfWord word =
    // TODO rest of the method
    let rec sumOfWord word =
        match word with 
        | [] -> 0
        | (index, set)::xt -> (snd (set |> Set.toList).[0]) + (sumOfWord xt)
    sumOfWord word

let createMove word (startPos:coord) goX goY =
    word
    // TODO rest of the method
    |> List.map (fun (a , x) -> (uint32 a, (Set.toList x).[0]))
    |> List.fold (fun ((x, y), c) value -> ((x + goX, y + goY), ((x,y), value)::c) ) (startPos, [])
    |> function | (_, x) -> x
    |> SMPlay

let createMoveFromListOfWords startPos goX goY describedWords =
    match describedWords with
    | [] -> SMPass
    | word::xt -> createMove word startPos goX goY
    
let foldAddIndexSetToSet (index, set) acc =
    // TODO rest of the method
    let rec aux result rest =
        match rest with
        | [] -> result
        | (char, points)::xt -> aux ((index, char, points)::result) xt
    
    (aux [] set) @ acc

let reverseMapKeyValue pieces hand =
    hand
    // TODO rest of the method
    |> MultiSet.fold
        (fun acc index ammountAvailable ->
            [1u .. ammountAvailable]
            |> List.fold (fun acc2 _ ->
                acc2 @ [index, Map.find index pieces]) acc)
        []
    |> List.fold (fun acc (index, set) ->  foldAddIndexSetToSet (index, Set.toList set) acc) []
    |> List.sortBy (fun (index, char, points) -> points)
    |> List.map (fun (index, char, points) -> index, char)
    |> List.fold (fun acc (index, char) ->
        match Map.tryFind char acc with
        | None -> Map.add char [index] acc
        | Some lst -> Map.add char (index::lst) acc
        )
        Map.empty

let createWordsFromHand hand pieces= 
    hand
    // TODO rest of the method
    |> MultiSet.fold
        (fun acc index ammountAvailable ->
            [1u .. ammountAvailable]
            |> List.fold (fun acc2 _ ->
                acc2 @ [Map.find index pieces]) acc)
        []
    |> findAnagrams
    |> convertToListOfStrings

let createWordsFromStartChar hand pieces startCharLst length = 
    hand
    // TODO rest of the method
    |> MultiSet.fold
        (fun acc index ammountAvailable ->
            [1u .. ammountAvailable]
            |> List.fold (fun acc2 _ ->
                acc2 @ [Map.find index pieces]) acc)
        []
    |> findAnagrams
    |> convertToListOfStrings
    |> List.filter (fun string -> length >= string.Length)
    |> List.map (fun string -> (startCharLst |> charactersToString) + string)

let getAndRemoveIndexFromMap key (map : Map<'a, 'b list>) =
    
    let intList = Map.find key map
    let map = map |> Map.add key intList.Tail

    (intList.Head, map)

let convertStringToPiece words mapCharToIndexes pieces = 
    words |> List.map (fun word ->
        word
        // TODO rest of the method
        |> Seq.toList
        |> List.fold (
            fun (accRes, accmapCharToIndexes) c ->
            let (index, map) = getAndRemoveIndexFromMap c mapCharToIndexes
            (accRes @ [(index, Map.find index pieces)], map)
        )
            ([], mapCharToIndexes)
    )
    |> List.map (fun (x, map) -> x)

let findLegalWords words dictionary = 
    words
    |> List.filter (fun x -> Dictionary.lookup x dictionary)
    |> List.distinct

let placeOnEmptyBoard center pieces (state : State.state) dict =
    let mapCharToIndexes = reverseMapKeyValue pieces state.hand

    let words = createWordsFromHand state.hand pieces
    let legalWords = findLegalWords words dict 
    
    let wordsToPieceListList =
        convertStringToPiece legalWords mapCharToIndexes pieces
        |> List.sortByDescending (fun n -> pointSumOfWord n)
    
    wordsToPieceListList
    |> createMoveFromListOfWords center 1 0

let bestWord pieces hand charList length (dict:Dictionary.Dictionary) =
    // TODO rest of the method
    let mapCharToIndexes = reverseMapKeyValue pieces hand
    let words = createWordsFromStartChar hand pieces charList length
    let legalWords =
        findLegalWords words dict
        |> List.map (fun string -> string.Remove (0, (List.length charList)))

    convertStringToPiece legalWords mapCharToIndexes pieces
    |> List.map (fun x -> (pointSumOfWord x, x))
    |> List.sortByDescending (fun (sum, x) -> sum)

let charOnTile (coord:coord) placed =
    match Map.tryFind coord placed with
    | Some (c, _) -> c
    | None -> ' '

let isTileEmpty (coord:coord) lettersPlaces (board:ScrabbleUtil.board) boardRadius =
    match Map.tryFind coord lettersPlaces with
        | None -> true
        | Some _ -> false

let emptyPlaces (coord:coord) lettersPlaced (board:ScrabbleUtil.board) moveX moveY handSize boardRadius =
    // TODO rest of the method
    let handSize = int handSize
    let rec innerFn (coord:coord) value =
        if (value < handSize)
        then
            let newCoords = ((fst coord) + moveX, (snd coord) + moveY)
            let neighborCheck1 = isTileEmpty (fst newCoords + moveY, snd newCoords + moveX) lettersPlaced board boardRadius
            let neighborCheck2 = isTileEmpty (fst newCoords - moveY, snd newCoords - moveX) lettersPlaced board boardRadius
            let neighborIsOkay = neighborCheck1 && neighborCheck2
            match isTileEmpty newCoords lettersPlaced board boardRadius, neighborIsOkay with
            | true, true ->  (innerFn newCoords (value + 1))
            | false, _ -> value-1
            | true, false -> value
        else value
    innerFn coord 0

let wordAdjacentToTile (coord:coord) lettersPlaced (board:ScrabbleUtil.board) moveX moveY radius =
    // TODO rest of the method
    let rec innerFn (coord:coord) value =
        if (isTileEmpty coord lettersPlaced board radius) then value
        else 
            let newCoords = ((fst coord) + moveX, (snd coord) + moveY)
            let charOnTile = charOnTile coord lettersPlaced
            innerFn newCoords (charOnTile::value)
    innerFn coord []

let theTwoWordsAdjacentToTile (coord:coord) lettersPlaced (board:ScrabbleUtil.board) radius =
    wordAdjacentToTile coord lettersPlaced board -1 0 radius,
    wordAdjacentToTile coord lettersPlaced board 0 -1 radius

let PlaceOnNonEmptyBoard board pieces (state : State.state) radius (dict:Dictionary.Dictionary)=
    // helperMethods
    //let isTileEmpty (x,y) = isTileEmpty (x,y) state.lettersPlaced board radius
    //let charOnTile (x,y) = charOnTile (x,y) state.lettersPlaced board
    let handSize = state.hand |> MultiSet.fold (fun acc _ ammountAvailable -> ammountAvailable + acc) 0u
    let emptyPlaces (coord:coord) moveX moveY = emptyPlaces coord state.lettersPlaced board moveX moveY handSize radius
    
    let occupiedTiles : coord list =
        let letters = Map.toList state.lettersPlaced;
        let rec tileLocationsRec tail = 
            match tail with
            | [] -> []
            | (x : coord, y)::xx -> x::tileLocationsRec(xx)
        tileLocationsRec (letters)
            
    // map to tile with most empty places for each string
    let mapCharToBestTile =
        occupiedTiles
        // TODO rest of the piping
        |> List.map (
                fun coord -> 
                let freeSpacesX = emptyPlaces coord 1 0
                let freeSpacesY = emptyPlaces coord 0 1
                (coord, freeSpacesX, freeSpacesY)
            )
        |> List.filter (fun (coord, freeSpacesX, freeSpacesY) -> freeSpacesX > 0 || freeSpacesY > 0)
        |> List.map
            (fun (coord, freeSpacesX, freeSpacesY) ->
                (theTwoWordsAdjacentToTile coord state.lettersPlaced board radius, (coord, freeSpacesX, freeSpacesY)))
        |> List.fold
            (fun acc ((stringX, stringY), ((x,y), freeSpacesX, freeSpacesY)) ->
                let addTo string placesX placesY acc = Map.add string ((x,y), placesX, placesY) acc
                
                let updateMap string _placesX _placesY acc = 
                    if _placesX = -1 && _placesY = -1 then acc
                    else 
                        let tilesPlaces = max freeSpacesX freeSpacesY
                        match Map.tryFind string acc with
                        | None -> acc |> addTo string _placesX _placesY
                        | Some (_, plx, ply) -> if (max plx ply) < tilesPlaces then acc |> addTo string _placesX _placesY else acc

                if stringX = stringY
                then  acc |> updateMap stringX freeSpacesX freeSpacesY
                else  acc |> updateMap stringX freeSpacesX -1 |> updateMap stringY -1 freeSpacesY  
            )
            Map.empty

    let listBestChar =
        mapCharToBestTile
        // TODO rest of the piping
        |> Map.toList
        |> List.map (fun (charLst, ((x,y),  placesX, placesY)) -> 
            let words = bestWord pieces state.hand charLst (max placesX placesY) dict
            let word =
                match words with
                | [] -> None
                | xh::xt -> Some xh
            (charLst, word)
            )
        |> List.filter (fun (c, word) -> match word with | None -> false | Some _ -> true)
        |> List.map (fun (c, word) -> (c, word.Value))
        |> List.sortBy (fun (c, (sum, word)) -> sum)

    match listBestChar with
    | [] -> SMForfeit
    | (char, (sum, word))::_ -> 
        let ((x,y), placesX, placesY) = Map.find char mapCharToBestTile

        let distanceX, distanceY =
            if placesX >= placesY then (1,0)
            else (0,1)
        
        let x = distanceX + x
        let y = distanceY + y
        
        createMove word (x,y) distanceX distanceY

let makeMove (board:ScrabbleUtil.board) pieces (state : State.state) radius (dict:Dictionary.Dictionary) =
    match Map.tryFind (board.center) state.lettersPlaced with
    | None -> placeOnEmptyBoard (board.center) pieces state dict
    | Some _ -> PlaceOnNonEmptyBoard board pieces state radius dict

let createDictionary words =
    let englishAlfabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    List.fold (fun acc s -> Dictionary.insert s acc) (Dictionary.empty englishAlfabet) words

// From Jesper. But adjusted just handle other input and works with a Dictionary
let playGame cstream board pieces (st : State.state) words =
    let dict = createDictionary words
    let lookup word =
        Dictionary.lookup word dict
        
    let rec aux (state : State.state) =
        Print.printBoard board 8 (State.lettersPlaced state)
        printfn "\n\n"
        Print.printHand pieces (State.hand state)

        printfn "Input move (format '(<x-coordinate><y-coordinate> <piece id><character><point-value> )*', note the absence of state between the last inputs)"
        let input =  System.Console.ReadLine()
        printf "Word: %A -> %A\n" input (lookup input)
        let move =
            match input with
            | "AI" -> makeMove board pieces state 8 dict
            | "PASS" -> SMPass
            | _ -> SMPass 
            //| "NEW_HAND" -> SMChange

        printfn "Trying to play: %A" move
        send cstream (move)
        let msg = recv cstream
        match msg with
        | RCM (CMPlaySuccess(moves, points, newPieces)) ->
            (* Successful play by you. Update your state *)
            printfn "Success. You moved: %A" moves
            printfn "points: %A" points
            printfn "new pieces %A" newPieces
            
            let newState = state |> (State.addPlacedPiecesToBoard moves
                         >> State.removePiecesFromHand moves
                         >> State.addPiecesToHand newPieces) 
            aux newState
        | RCM (CMPlayed (pid, moves, points)) ->
            (* Successful play by other player. Update your state *)
            printfn "Player %A, played:\n %A" pid moves
            let newState = state |> State.addPlacedPiecesToBoard moves
            aux newState
        | RCM (CMPlayFailed (pid, ms)) ->
            (* Failed play. Update your state *)
            let newState = state // This state needs to be updated
            aux newState
        | RCM (CMGameOver _) -> ()
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RErr err -> printfn "Server Error:\n%A" err; aux state
        | RGPE err -> printfn "Gameplay Error:\n%A" err; aux state

    aux st

// From Jesper
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

// From Jesper
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

// From Jesper
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

// From Jesper
[<EntryPoint>]
let main argv =
    [Comm.startServer 13000; startGame 13000 1u] |>
    Async.Parallel |>
    Async.RunSynchronously |> ignore
    0 // return an integer exit code