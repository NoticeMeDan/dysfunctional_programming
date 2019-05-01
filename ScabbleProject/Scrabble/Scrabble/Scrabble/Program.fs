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
    let printPoints points names =
        points
        |> Map.toList
        |> List.iter(fun (k,v) ->  printfn "player %A have points: %A" (Map.find k names) v )
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
    open ScrabbleUtil

    type state = {
        lettersPlaced : Map<ScrabbleUtil.coord, char * int>
        hand          : MultiSet.MultiSet<uint32>
        dictionary    : Dictionary.Dictionary
        ourID         : uint32
        playerTurn    : uint32          //todo
        playersScores : Map<uint32, uint32> //todo
        playersName   : Map<uint32, string> //todo
    }
    let makeDictionary = 
        let readLines filePath = System.IO.File.ReadLines filePath
        let fromFile path = 
            let ed = Dictionary.empty ""
            Seq.foldBack Dictionary.insert (readLines path) ed
        let englishDict = fromFile "../../../EnglishDictionary.txt"
        englishDict;

    let mkState lp h = {
        lettersPlaced = lp; 
        hand          = h; 
        dictionary    = makeDictionary; 
        ourID         = 0u; 
        playerTurn    = 0u; 
        playersScores = Map.empty; 
        playersName   = Map.empty
    }

    let newState hand = mkState Map.empty hand

    let lettersPlaced st = st.lettersPlaced
    let hand st          = st.hand


module CalculatePoints =
    let rec flatten lstst = 
        match lstst with
        | [] -> []
        | xh::xt -> xh @ (flatten xt)
    let calculatePoints (lst: tile list) (ws : (char * int)[]) : int =
        let convert =
            lst
            |> List.indexed
            |> List.map (fun (index, (ch, map)) ->
                    map
                    |> Map.map (fun k tile ->
                        tile (uint32 index) ws )
                )
        convert
        |> List.map(fun m -> m |> Map.toList)
        |> flatten
        |> List.sortBy (fun (p,_) -> p)
        |> List.fold (fun state (_, f) -> f state) 0

let handleMessageFromServer play (st: State.state) msg =
    match msg with
    | RCM (CMPlaySuccess(ms, points, newPieces)) ->
        (* Successful play by you. Update your state *)
        let st1 = {st with lettersPlaced = 
                                 ms
                                 |> List.fold (fun acc (coordinate, (id, charVal)) -> acc |> Map.add coordinate charVal) st.lettersPlaced}
        let st2 = {st1 with hand = 
                                 ms
                                 |> List.fold (fun acc (coordinate, (id, charVal)) -> acc |> MultiSet.remove id 1u) st1.hand } 
        let st3 = {st2 with hand = 
                                 newPieces
                                 |> List.fold (fun acc (id, amount) -> acc |> MultiSet.add id amount) st2.hand } 
        let st4 = {st3 with playersScores = st3.playersScores |> Map.add st3.ourID ((st3.playersScores |> Map.find st3.ourID) + (points |> uint32))} 
        play st4
    | RCM(CMPassed (playerID))  -> play st
    | RCM(CMForfeit (playerID))  ->
        let st1 = {st with playersScores = st.playersScores |> Map.remove playerID }
        play st
    | RCM (CMPlayed (pid, ms, points)) ->
        (* Successful play by other player. Update your state *)
        let st1 = {st with lettersPlaced = 
                                 ms
                                 |> List.fold (fun acc (coordinate, (id, charVal)) -> acc |> Map.add coordinate charVal) st.lettersPlaced}
        let st2 = {st1 with playersScores = st1.playersScores |> Map.add pid ((st1.playersScores |> Map.find pid) + (points |> uint32))}         
        play st2
    | RCM (CMPlayFailed (pid, ms)) ->
        (* Failed play. Update your state *)
        let st' = st // This state needs to be updated
        play st'
    | RCM (CMGameOver _) -> ()
    | RCM a -> failwith (sprintf "not implmented: %A" a)
    | RErr err -> printfn "Server Error:\n%A" err; play st
    | RGPE err -> printfn "Gameplay Error:\n%A" err; play st

    
let rec makeCombinations (lst ) =
    let givenLenghtOflst = List.length lst
    
    let rec innerFunc currentWord index map=
        if List.length currentWord < givenLenghtOflst then
            let nextCurrentWord = currentWord @ [lst.[index]]
            let newMap = map |> Map.add index index 
            [0 .. (givenLenghtOflst-1)]
            |> List.fold (fun acc value ->
                match Map.tryFind value newMap with
                | None -> (innerFunc nextCurrentWord value newMap) @ acc
                | Some _ -> acc
                ) [nextCurrentWord]
        else []

    [0 .. (givenLenghtOflst-1)] |> List.fold (fun acc value -> (innerFunc [] value Map.empty)@acc) []

let charListToString (cl:char list) = List.foldBack (fun x acc -> x.ToString() + acc) cl ""
let rec setCharIntListToCharList lst =
    match lst with
    | [] -> []
    | (char : Set<char*int>) :: xtt ->
        let x =
            char
            |> Set.map (fun (c, i)->c)
            |> Set.toArray
        [x.[0]] @ (setCharIntListToCharList xtt)        //todo don't just take the first


let convertToListOfStrings (lst : Set<char*int> list list) =
    lst
    |>List.map (fun x -> x |> setCharIntListToCharList |> charListToString)

let rec sumOfWord word =
    match word with 
    | [] -> 0
    | (index, set)::xt -> (snd (set |>Set.toList).[0]) + (sumOfWord xt) 


let createMove word startPos goX goY =
    word
    |> List.map (fun (a ,x) -> (uint32 a, (Set.toList x).[0]))
    |> List.fold (fun ((x, y), c) value -> ((x + goX, y + goY), ((x,y), value)::c) ) (startPos, [])
    |>function | (_, x) -> x
    |> SMPlay

let createMoveFromListOfWords startPos goX goY describedWords =
    match describedWords with
    | [] -> SMPass
    | word::xt ->   createMove word startPos goX goY
let foldAddIndexSetToSet (index, set) acc =
    let rec aux result rest =
        match rest with
        | [] -> result
        | (char, points)::xt -> aux ((index, char, points)::result) xt
    
    (aux [] set) @ acc

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


let createWordCombinationsInHand hand pieces= 
    hand
    |> MultiSet.fold
        (fun acc index ammountAvailable ->
            [1u .. ammountAvailable]
            |> List.fold (fun acc2 _ ->
                acc2 @ [Map.find index pieces]) acc)
        []
    |> makeCombinations
    |> convertToListOfStrings

let createWordCombinationsInHandFromStartChar hand pieces startCharLst lenght= 
    hand
    |> MultiSet.fold
        (fun acc index ammountAvailable ->
            [1u .. ammountAvailable]
            |> List.fold (fun acc2 _ ->
                acc2 @ [Map.find index pieces]) acc)
        []
    |> makeCombinations
    |> convertToListOfStrings
    |> List.filter (fun string -> lenght >= string.Length)
    |> List.map (fun string -> (startCharLst |> charListToString) + string)

let getAndRemoveIndexFromMap key (map : Map<'a, 'b list>) =
    
    let intList = Map.find key map
    let map = map |> Map.add key intList.Tail

    (intList.Head, map)

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

let filterWords (st : State.state) words= 
    words
    |> List.filter (fun x -> Dictionary.lookup x st.dictionary)
    |> List.distinct

let placeOnEmptyBoard center pieces (st : State.state) hand = 
    let mapCharToIndexes = reversePiecesMap pieces hand
    printfn "%A" mapCharToIndexes

    let words = createWordCombinationsInHand hand pieces
    let filteredWords = words |> filterWords st
    printfn "filteredWords: %A" filteredWords
    
    let describedWords =
        convertStringToPiece filteredWords mapCharToIndexes pieces
        |> List.sortByDescending (fun x -> sumOfWord x)
    
    describedWords
    |> createMoveFromListOfWords center 1 0

let bestExtendingWord pieces (st : State.state) hand charLst lenght = 
    let mapCharToIndexes = reversePiecesMap pieces hand
    let words = createWordCombinationsInHandFromStartChar hand pieces charLst lenght
    let filteredWords =
        words
        |> filterWords st
        |> List.map (fun string -> string.Remove (0, (List.length charLst)))

    convertStringToPiece filteredWords mapCharToIndexes pieces
    |> List.map (fun x -> (sumOfWord x, x))
    |> List.sortByDescending (fun (sum, x) -> sum)

let charOnTile (x,y) placed board=
    match Map.tryFind (x, y) placed, ScrabbleUtil.Board.tiles board (x, y) with
    | None, Some (c, _) -> c
    | Some (c, _), _    -> c
    | _, None -> ' '

let isTileEmpty (x,y) placed board =
    let c = charOnTile (x,y) placed board
    (' ' = c ) || (System.Char.IsLower c)

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

let emptyLeft (x,y) placed board = isTileEmpty (x-1,y) placed board
let emptyAbove (x,y) placed board = isTileEmpty (x,y-1) placed board


let wordAdjacentToTile (x,y) placed board moveX moveY =
    let rec innerFn (x,y) value =
        if (isTileEmpty (x,y) placed board ) then value
        else 
            let newCoords = (x + moveX, y + moveY)
            let charOnTile = charOnTile (x,y) placed board
            innerFn newCoords (charOnTile::value)
    innerFn (x,y) []

let theTwoWordsAdjacentToTile (x,y) placed board =
    wordAdjacentToTile (x,y) placed board -1 0, wordAdjacentToTile (x,y) placed board 0 -1

let PlaceOnNonEmptyBoard board pieces (st : State.state) radius placed hand=
    // helperMethods
    let isTileEmpty (x,y) = isTileEmpty (x,y) placed board
    let charOnTile (x,y) = charOnTile (x,y) placed board
    let handSize = hand |> MultiSet.fold (fun acc _ ammountAvailable -> ammountAvailable + acc) 0u
    let emptyPlaces (x,y) moveX moveY = emptyPlaces (x,y) placed board moveX moveY handSize
    
    // board size
    let c = ScrabbleUtil.Board.center board
    let minX = fst c - radius
    let maxX = fst c + radius
    let minY = snd c - radius
    let maxY = snd c + radius


    // AI
    let occupiedTileLocations = [
            for y in [minY..maxY] do
                for x in [minX..maxX] do
                    if (isTileEmpty (x,y))
                        then ()
                        else yield (x,y)
         ]   

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
// how to play multiplayer
// place letter in the middle of a word - see comment at the line 408
// singleletter score, double, etc


// mapCharToBestTile should go from type map<char list, (int*int)*int*int) to map<char list, (int*int)*(int*int)*(int*int))
// listBestChar should go from type 'a list  to ('a * 'a ) list
// bestExtendingWord shuld be refractored

    let listBestChar =
        mapCharToBestTile
        |> Map.toList
        |> List.map (fun (charLst, ((x,y),  placesX, placesY)) -> 
            let words = bestExtendingWord pieces st hand charLst (max placesX placesY)
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
    | [] -> SMPass
    | (char, (sum, word))::_ -> 
        let ((x,y), placesX, placesY) = Map.find char mapCharToBestTile

        let distanceX, distanceY =
            if placesX >= placesY then (1,0)
            else (0,1)
        
        let x = distanceX + x
        let y = distanceY + y
        
        createMove word (x,y) distanceX distanceY

let AIDecideMove board pieces (st : State.state) radius placed hand=
    // type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>
    //type board = { center : coord; usedTile : tile; tiles : coord -> tile option }
    match Map.tryFind (board.center) placed, ScrabbleUtil.Board.tiles board (board.center) with
    | None, Some (' ', map) ->      
        placeOnEmptyBoard (board.center) pieces st hand
    | _, _    ->                                                   
        PlaceOnNonEmptyBoard board pieces (st : State.state) radius placed hand



let playGame cstream board pieces (st : State.state) =

    let rec aux (st : State.state) =
        Print.printBoard board 8 (State.lettersPlaced st)
        printfn "\n\n"
        Print.printPoints st.playersScores st.playersName
        printfn "\n\n"
        Print.printHand pieces (State.hand st)

        
        let player = "AI"
        let play = 
            match player with
            | "AI" -> AIDecideMove board pieces st 8  (State.lettersPlaced st) st.hand
            | "SKIP" -> SMPass
            | _    ->
                    printfn "Input move (format '(<x-coordinate><y-coordinate> <piece id><character><point-value> )*', note the absence of state between the last inputs)"
                    let input =  System.Console.ReadLine()
                    let move = RegEx.parseMove input
                    SMPlay move
        printfn "Play: %A" play
        send cstream (play)
        let msg = recv cstream
        handleMessageFromServer aux st msg
    aux st

let setupGame cstream board alphabet words handSize timeout =
    let rec aux () =
        match ServerCommunication.recv cstream with
        | RCM (CMPlayerJoined name) ->
            printfn "Player %s joined" name
            aux ()
        | RCM (CMGameStarted (playerNumber, hand, firstPlayer, pieces, players)) as msg ->
            printfn "Game started %A" msg
            let state = 
                hand
                |> List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty
                |> State.newState
            let state = {state with ourID = playerNumber}
            let state = {state with playersScores = players |> List.map(fun (id, name) -> (id, 0u)) |> Map.ofList}
            let state = {state with playersName = players |> Map.ofList}
            let state = {state with playerTurn = firstPlayer}
            playGame cstream board pieces state

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

        send cstream (SMStartGame (numberOfPlayers, "My game", "password", "My player", seed, board, pieces,
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