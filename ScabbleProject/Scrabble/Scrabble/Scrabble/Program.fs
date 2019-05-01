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
    
    // TODO
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
    // TODO
    let addPlacedPiecesToBoard (pieces:piecePlaced list) (state:state) =
        let updateLettersPlaced =
                List.fold (fun lettersPlaced (coord, (_, piece)) ->
                Map.add coord piece lettersPlaced) state.lettersPlaced pieces
        overwriteLettersPlaced state updateLettersPlaced
    
    // Add pieces to hand and return the updated state
    // TODO
    let addPiecesToHand (pcs:(uint32*uint32) list) (st:state) =
        let hand' = List.fold (fun acc (pid, x) -> MultiSet.add pid x acc) st.hand pcs
        overwriteHand st hand'
        
    // Remove pieces from hand, given a list of moves played, and return the updated state
    // TODO
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
    
// TODO
let charListToString (cl:char list) = List.foldBack (fun x acc -> x.ToString() + acc) cl ""

// TODO
let rec setCharIntListToCharList lst =
    match lst with
    | [] -> []
    | (char : Set<char*int>) :: xtt ->
        let x =
            char
            |> Set.map (fun (c, i)->c)
            |> Set.toArray
        [x.[0]] @ (setCharIntListToCharList xtt)        //todo don't just take the first

// TODO
let convertToListOfStrings (lst : Set<char*int> list list) =
    lst
    |>List.map (fun x -> x |> setCharIntListToCharList |> charListToString)

// TODO
let rec sumOfWord word =
    match word with 
    | [] -> 0
    | (index, set)::xt -> (snd (set |>Set.toList).[0]) + (sumOfWord xt) 

// TODO
let createMove word startPos goX goY =
    word
    |> List.map (fun (a ,x) -> (uint32 a, (Set.toList x).[0]))
    |> List.fold (fun ((x, y), c) value -> ((x + goX, y + goY), ((x,y), value)::c) ) (startPos, [])
    |>function | (_, x) -> x
    |> SMPlay

// TODO
let createMoveFromListOfWords startPos goX goY describedWords =
    match describedWords with
    | [] -> SMPass
    | word::xt -> createMove word startPos goX goY
    
// TODO
let foldAddIndexSetToSet (index, set) acc =
    let rec aux result rest =
        match rest with
        | [] -> result
        | (char, points)::xt -> aux ((index, char, points)::result) xt
    
    (aux [] set) @ acc

// TODO
let reversePiecesMap pieces hand =
    //convert hand to list of (index, set list) list
    // list.fold  (index, set list) list ->  (index, char, points) list
    //sort points
    // map (index, char, points) list -> (char, index) list
    // list.fold (char, index) list -> map<char, index list>

    hand
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

// TODO
let createWordCombinationsInHand hand pieces= 
    hand
    |> MultiSet.fold
        (fun acc index ammountAvailable ->
            [1u .. ammountAvailable]
            |> List.fold (fun acc2 _ ->
                acc2 @ [Map.find index pieces]) acc)
        []
    |> findAnagrams
    |> convertToListOfStrings

// TODO
let createWordCombinationsInHandFromStartChar hand pieces startCharLst length= 
    hand
    |> MultiSet.fold
        (fun acc index ammountAvailable ->
            [1u .. ammountAvailable]
            |> List.fold (fun acc2 _ ->
                acc2 @ [Map.find index pieces]) acc)
        []
    |> findAnagrams
    |> convertToListOfStrings
    |> List.filter (fun string -> length >= string.Length)
    |> List.map (fun string -> (startCharLst |> charListToString) + string)

// TODO
let getAndRemoveIndexFromMap key (map : Map<'a, 'b list>) =
    
    let intList = Map.find key map
    let map = map |> Map.add key intList.Tail

    (intList.Head, map)

// TODO
let convertStringToPiece words mapCharToIndexes pieces = 
    words
    |> List.map (fun word ->
                        word
                        |> Seq.toList
                        |> List.fold
                            (fun (accRes, accmapCharToIndexes) c ->
                                      let (index, map) = getAndRemoveIndexFromMap c mapCharToIndexes
                                      (accRes @ [(index, Map.find index pieces)], map) )
                            ([], mapCharToIndexes)
                    )
    |> List.map (fun (x, map) -> x)

let filterWords words dictionary = 
    words
    |> List.filter (fun x -> Dictionary.lookup x dictionary)
    |> List.distinct

// TODO. Gave dictionary as argument
let placeOnEmptyBoard center pieces (state : State.state) hand dict = 
    let mapCharToIndexes = reversePiecesMap pieces hand
    printfn "%A" mapCharToIndexes

    let words = createWordCombinationsInHand hand pieces
    let filteredWords = filterWords words dict 
    printfn "filteredWords: %A" filteredWords
    
    let describedWords =
        convertStringToPiece filteredWords mapCharToIndexes pieces
        |> List.sortByDescending (fun x -> sumOfWord x)
    
    describedWords
    |> createMoveFromListOfWords center 1 0

// TODO
let bestExtendingWord pieces (st : State.state) hand charLst lenght (dict:Dictionary.Dictionary)= 
    let mapCharToIndexes = reversePiecesMap pieces hand
    let words = createWordCombinationsInHandFromStartChar hand pieces charLst lenght
    let filteredWords =
        filterWords words dict
        |> List.map (fun string -> string.Remove (0, (List.length charLst)))

    convertStringToPiece filteredWords mapCharToIndexes pieces
    |> List.map (fun x -> (sumOfWord x, x))
    |> List.sortByDescending (fun (sum, x) -> sum)

// TODO
let charOnTile (x,y) placed board=
    match Map.tryFind (x, y) placed, ScrabbleUtil.Board.tiles board (x, y) with
    | None, Some (c, _) -> c
    | Some (c, _), _    -> c
    | _, None -> ' '

// TODO
let isTileEmpty (x,y) placed board =
    let c = charOnTile (x,y) placed board
    (' ' = c ) || (System.Char.IsLower c)

// TODO
let emptyPlaces (x,y) placed board moveX moveY handSize =
    let handSize = int handSize
    let rec innerFn (x,y) value =
        if (value < handSize)
        then
            let newCoords = (x + moveX, y + moveY)
            let neighborCheck1 = isTileEmpty (fst newCoords + moveY, snd newCoords + moveX) placed board
            let neighborCheck2 = isTileEmpty (fst newCoords - moveY, snd newCoords - moveX) placed board
            let neighborIsOkay = neighborCheck1 && neighborCheck2
            match isTileEmpty newCoords placed board , neighborIsOkay with
            | true, true ->  (innerFn newCoords (value + 1))
            | false, _ -> value-1
            | true, false -> value
        else value
    innerFn (x,y) 0

// TODO
let wordAdjacentToTile (x,y) placed board moveX moveY =
    let rec innerFn (x,y) value =
        if (isTileEmpty (x,y) placed board ) then value
        else 
            let newCoords = (x + moveX, y + moveY)
            let charOnTile = charOnTile (x,y) placed board
            innerFn newCoords (charOnTile::value)
    innerFn (x,y) []

// TODO
let theTwoWordsAdjacentToTile (x,y) placed board =
    wordAdjacentToTile (x,y) placed board -1 0, wordAdjacentToTile (x,y) placed board 0 -1

// TODO. Gave dictionary as argument
let PlaceOnNonEmptyBoard board pieces (state : State.state) radius placed hand (dict:Dictionary.Dictionary)=
    // helperMethods
    let isTileEmpty (x,y) = isTileEmpty (x,y) placed board
    let charOnTile (x,y) = charOnTile (x,y) placed board
    let handSize = hand |> MultiSet.fold (fun acc _ ammountAvailable -> ammountAvailable + acc) 0u
    let emptyPlaces (x,y) moveX moveY = emptyPlaces (x,y) placed board moveX moveY handSize
    
    // board size
    let center = ScrabbleUtil.Board.center board
    let minX = fst center - radius
    let maxX = fst center + radius
    let minY = snd center - radius
    let maxY = snd center + radius

    // AI
    let occupiedTileLocations = [
            for y in [minY..maxY] do
                for x in [minX..maxX] do
                    if (isTileEmpty (x,y))
                        then ()
                        else yield (x,y)
         ]
    
    //let tileLocations = [ for (KeyValue(v, _)) in state.lettersPlaced -> v ]
    
    // map to tile with most empty places for each string
    let mapCharToBestTile =
        occupiedTileLocations
        |> List.map (fun (x,y) -> 
            let placesX = emptyPlaces (x,y) 1 0
            let placesY = emptyPlaces (x,y) 0 1
            ((x,y), placesX, placesY)
            )
        |> List.filter (fun ((x,y), placesX, placesY) -> placesX > 0 || placesY > 0)
        |> List.map
            (fun ((x,y), placesX, placesY) ->
                (theTwoWordsAdjacentToTile (x,y) placed board, ((x,y), placesX, placesY)))
        |> List.fold
            (fun acc ((stringX, stringY), ((x,y), placesX, placesY)) ->
                let addTo string placesX placesY acc = Map.add string ((x,y), placesX, placesY) acc
                
                let updateMap string _placesX _placesY acc = 
                    if _placesX = -1 && _placesY = -1 then acc
                    else 
                        let tilesPlaces = max placesX placesY
                        match Map.tryFind string acc with
                        | None -> acc |> addTo string _placesX _placesY
                        | Some (_, plx, ply) -> if (max plx ply) < tilesPlaces then acc |> addTo string _placesX _placesY else acc

                if stringX = stringY
                then  acc |> updateMap stringX placesX placesY
                else  acc |> updateMap stringX placesX -1 |> updateMap stringY -1 placesY  
            )
            Map.empty
    
// todos:
// place letter in the middle of a word - see comment at the line 408
// singleletter score, double, etc

// mapCharToBestTile should go from type map<char list, (int*int)*int*int) to map<char list, (int*int)*(int*int)*(int*int))
// listBestChar should go from type 'a list  to ('a * 'a ) list
// bestExtendingWord shuld be refractored

    // TODO
    let listBestChar =
        mapCharToBestTile
        |> Map.toList
        |> List.map (fun (charLst, ((x,y),  placesX, placesY)) -> 
            let words = bestExtendingWord pieces state hand charLst (max placesX placesY) dict
            let word =
                match words with
                | [] -> None
                | xh::xt -> Some xh
            (charLst, word)
            )
        |> List.filter (fun (c, word) -> match word with | None -> false | Some _ -> true)
        |> List.map (fun (c, word) -> (c, word.Value))
        |> List.sortBy (fun (c, (sum, word)) -> sum)

    // TODO
    match listBestChar with
    | [] -> SMPass
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
    | None ->      
        placeOnEmptyBoard (board.center) pieces state state.hand dict
    | Some _ ->                                                   
        PlaceOnNonEmptyBoard board pieces state radius state.lettersPlaced state.hand dict

let AIDecideMove board pieces (st : State.state) radius placed hand (dict:Dictionary.Dictionary)=
    // type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>
    //type board = { center : coord; usedTile : tile; tiles : coord -> tile option }
    match Map.tryFind (board.center) placed, ScrabbleUtil.Board.tiles board (board.center) with
    | None, Some (' ', map) ->      
        placeOnEmptyBoard (board.center) pieces st hand dict
    | _, _    ->                                                   
        PlaceOnNonEmptyBoard board pieces (st : State.state) radius placed hand dict

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