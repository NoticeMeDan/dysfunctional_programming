open System.IO

open System
open MultiSet
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
    
    let mkState lp hand pieces = { lettersPlaced = lp; hand = hand; pieces = pieces }
    let newState hand = mkState Map.empty hand
    let lettersPlaced state = state.lettersPlaced
    let overwriteLetters state newLettersPlace = mkState newLettersPlace state.hand state.pieces
    let overwriteHand state newHand = mkState state.lettersPlaced newHand state.pieces
    
    // Add placed pieces to the local board state and return the updated state
    // TODO
    let addPlacedPiecesToBoard (pcs:piecePlaced list) (st:state) =
        let lettersPlaced' =
            List.fold (fun lettersPlaced (coord, (_, piece)) ->
                Map.add coord piece lettersPlaced) st.lettersPlaced pcs
        overwriteLetters st lettersPlaced'
    
    // Add pieces to hand and return the updated state
    // TODO
    let addPiecesToHand (pcs:(uint32*uint32) list) (st:state) =
        let newHand = List.fold (fun acc (id, x) -> MultiSet.add id x acc) st.hand pcs
        overwriteHand st newHand
        
    // Remove pieces from hand, given a list of moves played, and return the updated state
    // TODO
    let removePiecesFromHand (usedPiecesList:piecePlaced list) st =
        let newHand = List.fold (fun acc (_, (id, _)) -> MultiSet.removeSingle id acc) st.hand usedPiecesList
        overwriteHand st newHand
        
    let removeSwappedPiecesFromhand (pieces: (uint32 * uint32) list) (state: state) =
        let newHand = List.fold (fun acc (id, amount) -> MultiSet.remove id amount acc) state.hand pieces
        overwriteHand state newHand

let rec createAnagram list=
    let lengthOfList = List.length list-1
    let array = [0 .. (lengthOfList - 1)]
    
    let rec anagram wordArray index map =
        if List.length wordArray < lengthOfList then
            let nextWord = wordArray @ [list.[index]]
            let newMap = map |> Map.add index index 
            
            array |> List.fold (fun acc value ->
                match Map.tryFind value newMap with
                | None -> (anagram nextWord value newMap) @ acc
                | Some _ -> acc
                ) [nextWord]
        else []

    array |> List.fold (fun acc value -> (anagram [] value Map.empty)@acc) []
    
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
    | (index, set)::xt -> (snd (set |> Set.toList).[0]) + (sumOfWord xt) 

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
    | word::_ ->   createMove word startPos goX goY

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

// TODO
let createWordCombinationsInHand hand pieces= 
    hand
    |> MultiSet.fold
        (fun acc index ammountAvailable ->
            [1u .. ammountAvailable]
            |> List.fold (fun acc2 _ ->
                acc2 @ [Map.find index pieces]) acc)
        []
    |> createAnagram
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
    |> createAnagram
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

// TODO. Gave dictionary as argument
let filterWords (st : State.state) words dictionary = 
    words
    |> List.filter (fun x -> Dictionary.lookup x dictionary)
    |> List.distinct

// TODO. Gave dictionary as argument
let placeOnEmptyBoard center pieces (st : State.state) hand dict = 
    let mapCharToIndexes = mapPiecesToIndexes pieces hand
    printfn "%A" mapCharToIndexes

    let words = createWordCombinationsInHand hand pieces
    let filteredWords = filterWords st words dict 
    printfn "filteredWords: %A" filteredWords
    
    let describedWords =
        convertStringToPiece filteredWords mapCharToIndexes pieces
        |> List.sortByDescending (fun x -> sumOfWord x)
    
    describedWords
    |> createMoveFromListOfWords center 1 0

// TODO
let bestExtendingWord pieces (st : State.state) hand charLst lenght (dict: Dictionary.Dictionary) = 
    let words = createWordCombinationsInHandFromStartChar hand pieces charLst lenght
    let filteredWords =
        filterWords st words dict
        |> List.map (fun string -> string.Remove (0, (List.length charLst)))

    convertStringToPiece filteredWords (mapPiecesToIndexes pieces hand) pieces
    |> List.map (fun x -> async { return sumOfWord x, x })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.toList
    |> List.sortByDescending (fun (sum, x) -> sum)

// TODO
let charOnTile (x,y) placed board =
    match Map.tryFind (x, y) placed, ScrabbleUtil.Board.tiles board (x, y) with
    | None, Some (c, _) -> c
    | Some (c, _), _    -> c
    | _, None -> ' '

// TODO
let isTileEmpty (x,y) placed board boardRadius =
    let c = charOnTile (x,y) placed board
    ((' ' = c ) || (System.Char.IsLower c)) && x < boardRadius+1 && y < boardRadius+1 

let emptyPlacesInDirection (coord:coord) (state:State.state) board moveX moveY boardRadius =
    let placedPieces = state.lettersPlaced
    let handSize = state.lettersPlaced |> Map.toList |> List.length
    
    let newCoordinate coord = ((fst coord) + moveX, (snd coord) + moveY)
    
    let neighbourCheck coord =
        let neighborCheck1 = isTileEmpty (fst coord + moveY, snd coord + moveX) placedPieces board boardRadius
        let neighborCheck2 = isTileEmpty (fst coord - moveY, snd coord - moveX) placedPieces board boardRadius
        neighborCheck1 && neighborCheck2
    
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

// TODO
let emptyLeft (x,y) placed board = isTileEmpty (x-1,y) placed board

// TODO
let emptyAbove (x,y) placed board = isTileEmpty (x,y-1) placed board

// TODO
let wordAdjacentToTile (x,y) placed board moveX moveY radius =
    let rec innerFn (x,y) value =
        if (isTileEmpty (x,y) placed board radius) then value
        else 
            let newCoords = (x + moveX, y + moveY)
            let charOnTile = charOnTile (x,y) placed board
            innerFn newCoords (charOnTile::value)
    innerFn (x,y) []

// TODO
let findNeighbouringWords (x,y) placed board radius =
    wordAdjacentToTile (x,y) placed board -1 0 radius, wordAdjacentToTile (x,y) placed board 0 -1 radius

// TODO. Gave dictionary as argument
let PlaceOnNonEmptyBoard board pieces (st : State.state) radius placed hand (dict:Dictionary.Dictionary)=
    // helperMethods
    let isTileEmpty (x,y) = isTileEmpty (x,y) placed board radius
    let handSize = hand |> MultiSet.fold (fun acc _ ammountAvailable -> ammountAvailable + acc) 0u
    let emptyPlaces (x,y) moveX moveY = emptyPlacesInDirection (x,y) st board moveX moveY radius
    
    // board size
    let c = ScrabbleUtil.Board.center board
    let minX = fst c - radius
    let maxX = fst c + radius
    let minY = snd c - radius
    let maxY = snd c + radius

    // AI
    let findOccupiedTiles : coord list =
        let letters = Map.toList st.lettersPlaced;
        let rec tileLocationsRec tail = 
            match tail with
            | [] -> []
            | (x : coord, y)::xx -> x::tileLocationsRec(xx)
        tileLocationsRec (letters)   
    
    let mapEmptyPlaces list =
        List.map (fun (x,y) -> 
            let xs = emptyPlaces (x,y) 1 0
            let ys = emptyPlaces (x,y) 0 1
            ((x,y), xs, ys)) list
    
    let mapNeighbouringWords list =
        List.map (fun ((x,y), placesX, placesY) ->
            (findNeighbouringWords (x,y) placed board radius, ((x,y), placesX, placesY))) list
    
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
            
    // map to tile with most empty places for each string
    let mapWordToTiles =
        findOccupiedTiles
        |> mapEmptyPlaces
        |> mapNeighbouringWords
        |> update
    
    let findBestWords list =
        List.map (fun (charLst, ((x,y),  placesX, placesY)) -> 
            let words = bestExtendingWord pieces st hand charLst (max placesX placesY) dict
            let fstWord =
                match words with
                | [] -> None
                | fst::rst -> Some fst
            (charLst, fstWord)
            ) list
        
    let findPlayableMoves =
        Map.toList mapWordToTiles
        |> findBestWords
        |> List.filter (fun ( _ , word) -> match word with | None -> false | Some _ -> true)
        |> List.map (fun (charLst, word) -> (charLst, word.Value))
        |> List.sortBy (fun (charLst, (sum, word)) -> sum)

    match findPlayableMoves with
    | [] -> SMForfeit
    | (charLst, (sum, word))::_ -> 
        let ((x,y), placesX, placesY) = Map.find charLst mapWordToTiles
        
        let distanceX, distanceY =
            if placesX >= placesY then (1,0)
            else (0,1)
        
        let x = distanceX + x
        let y = distanceY + y
        
        createMove word (x,y) distanceX distanceY

// TODO
let AIDecideMove board pieces (st : State.state) radius placed hand (dict:Dictionary.Dictionary)=
    // type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>
    //type board = { center : coord; usedTile : tile; tiles : coord -> tile option }
    match Map.tryFind (board.center) placed, ScrabbleUtil.Board.tiles board (board.center) with
    | None, Some (' ', _) ->      
        placeOnEmptyBoard (board.center) pieces st hand dict
    | _, _    ->                                                   
        PlaceOnNonEmptyBoard board pieces (st : State.state) radius placed hand dict

let createDictionary words =
    let englishAlfabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    List.fold (fun acc s -> Dictionary.insert s acc) (Dictionary.empty englishAlfabet) words

// From Jesper. But adjusted just handle other input and works with a Dictionary
let playGame cstream board pieces (st : State.state) words =
    let dict = createDictionary words
        
    let rec aux (state : State.state) =
        Print.printBoard board 8 (State.lettersPlaced state)

        let hasWildcard = MultiSet.contains 0u state.hand
        
        let move =
            if hasWildcard
            then
                SMChange (List.replicate (int((MultiSet.numItems 0u state.hand))) 0u) // Swap the amount of wildcards on hand
            else
                AIDecideMove board pieces state 8  (State.lettersPlaced state) state.hand dict
            
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
        | RCM (CMChangeSuccess (newPieces)) ->
            (* Successful piece swap, update state *)
            printfn "Success. You swapped a piece."
            printfn "New piece(s): %A" newPieces
            
            // Remove wildcards, then add newPieces
            let newState = state |> (State.removeSwappedPiecesFromhand [(0u, MultiSet.numItems 0u state.hand)]
                                     >> State.addPiecesToHand newPieces )
            aux newState
        | RCM (CMPlayed (pid, moves, _)) ->
            (* Successful play by other player. Update your state *)
            printfn "Player %A, played:\n %A" pid moves
            let newState = state |> State.addPlacedPiecesToBoard moves
            aux newState
        | RCM (CMPlayFailed (_, _)) ->
            (* Failed play. Update your state *)
            let newState = state // This state needs to be updated
            aux newState
        | RCM (CMGameOver _) -> ()
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RErr err -> printfn "Server Error:\n%A" err; aux state
        | RGPE err -> printfn "Gameplay Error:\n%A" err; aux state

    aux st

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