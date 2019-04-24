module JParsec

open System

type Position = {
    line : int
    column : int
}

module Position =
    (* Module and datatype inspired by Scott Wlaschin's web-site
       F# for fun and profit *)

    let mkPos l c   = {line = l; column = c}
    let getLine p   = p.line
    let getColumn p = p.column

    let setLine l p  = {p with line = l}
    let setColumn l p = {p with column = l}

    let initialPos = mkPos 0 0

    let incrCol p = setColumn (getColumn p + 1) p
    let incrLine p = (setLine (getLine p + 1) >> setColumn 0) p



type TextInputState = {
    lines : string[]
    position : Position 
}

module TextInputState =

    let initialInputState s =
        if String.IsNullOrEmpty(s) then
            {lines=[||]; position=Position.initialPos}
        else
            let separators = [| "\r\n"; "\n" |]
            let lines = s.Split(separators, StringSplitOptions.None)
            {lines=lines; position=Position.initialPos} 

    let getLines    is = is.lines
    let getPosition is = is.position

    let setPosition is p = {is with position = p}

    let numLines = getLines >> Array.length

    let columnNumber = getPosition >> Position.getColumn
    let lineNumber   = getPosition >> Position.getLine

    let getLine is = 
        function
        | i when i < numLines is -> Some (getLines is).[i]
        | _ -> None

    let currentLine is = 
        getPosition is |> 
        Position.getLine |> 
        getLine is

    let getError is =
        let failureCaret = sprintf "%*s^" (columnNumber is) ""
        sprintf "Line: %i\tColumn: %i\n%s\n%s" 
                (lineNumber is) (columnNumber is) 
                (currentLine is |> Option.get) failureCaret

    let incrCol  is = getPosition is |> Position.incrCol |> setPosition is
    let incrLine is = getPosition is |> Position.incrLine |> setPosition is

    let nextChar is =
        let cPos = columnNumber is

        match currentLine is with
        | Some l when cPos < l.Length -> incrCol is, Some l.[cPos]
        | Some l                      -> incrLine is, Some '\n'
        | None                        -> is, None
                

type Result<'a, 'b> = 
   | Success of 'a
   | Failure of 'b

type ParserLabel = string
type ParseError = string

type ParseError<'a> = {
    label : ParserLabel
    input : 'a
    error : ParseError
}

module ParseError =
    let mkParseError l i err = {label = l; input = i; error = err}

    let setLabel pe l = {pe with label = l}

    let getLabel pe = pe.label
    let getInput pe = pe.input
    let getError pe = pe.error

    let print pe = 
        printf "Error parsing %s\n%s%s" 
                (getLabel pe) (TextInputState.getError (getInput pe)) (getError pe)
///<typeparam name="a'"> The input-type of the parser</typeparam>
/// <typeparam name="b'"> The output-type of the parser</typeparam>
type Parser<'a, 'b> = {
   pfun  : 'a -> Result<'b * 'a, ParseError<'a>>
   label : ParserLabel
}

module Parser =

    let printResult =
        function
        | Success (v, _) -> printfn "Success: %A" v
        | Failure pe -> ParseError.print pe

    let getSuccess =
        function
        | Success (v, _) -> Some v
        | _              -> None
    ///<summary>
    /// Creates a parser from a parsing-function and a label
    /// </summary>
    let mkParser f l = {pfun = f; label = l}
    ///<returns>The label of a parser</returns>
    let getLabel p = p.label
    /// <summary>
    /// Given a parser and a input, returns the result of running the parser on the input
    /// </summary>
    let run parser input =
        parser.pfun input
        
    let satisfy nextSymbol predicate label =
        let parseFun input =
            let remainingInput, symOpt = nextSymbol input
            match symOpt with
            | None     -> Failure (ParseError.mkParseError label input "no more input")
            | Some sym when predicate sym -> 
                  Success (sym, remainingInput)
            | Some sym -> 
                let err = sprintf "unexpected %A\n" sym
                Failure (ParseError.mkParseError label input err)

        mkParser parseFun label
    /// <summary>
    /// Creates a parser, that when run always returns a Success  with x and uncomsumed input
    /// </summary>
    let returnP x = mkParser (fun input -> Success(x, input)) "unknown"
    /// <summary>
    /// Wraps the function f in a parser, and binds it to the parser p
    /// </summary>
    let bindP f p = 
        let parseFun input =
            match run p input with
            | Failure pe -> Failure pe
            | Success (v, remainingInput) ->
                run (f v) remainingInput
        mkParser parseFun "unknown"

    /// <summary>
    /// Wraps the function f in a parser, and binds it to the parser p
    /// </summary>
    let (>>=) p f = bindP f p
    /// <summary>
    /// Creates a new parser from p, with the specified label l
    /// </summary>
    let setLabel p l =
        let parseFun input =
            match run p input with
            | Success s  -> Success s
            | Failure pe -> Failure (ParseError.setLabel pe l)

        mkParser parseFun l
    /// <summary>
    /// Creates a new parser from p, with the specified label l
    /// </summary>
    let (<?>) = setLabel
    /// <summary>
    /// Creates a new parser, which feeds the input to p1, and then
    /// feeds the output of p1 as input to p2, and returns the result of both parsers
    /// </summary>
    let andThen p1 p2 = 
        p1 >>= (fun r1 -> p2 >>= fun r2 -> returnP (r1, r2)) <?>
        sprintf "(%s andThen %s)" (getLabel p1) (getLabel p2)
    /// <summary>
    /// Creates a new parser, which feeds the input to p1, and then
    /// feeds the output of p1 as input to p2, and returns the result of both parsers
    /// </summary>
    let (.>>.) = andThen
    /// <summary>
    /// Creates a new parser, which feeds the input to p1, and returns if it succeeds,
    /// or else feeds the input to p2.
    /// </summary>
    let orElse p1 p2 =
        let label = sprintf "(%s orElse %s)" (getLabel p1) (getLabel p2)
        let parserFun input =
            match run p1 input with
            | Success s  -> Success s
            | Failure pe -> run p2 input

        mkParser parserFun label
    /// <summary>
    /// Creates a new parser, which feeds the input to p1, and returns if it succeeds,
    /// or else feeds the input to p2.
    /// </summary>
    let (<|>) = orElse
    /// <summary>
    /// Takes a parser whose output is a function, and a parser whose output is a value,
    /// and creates a parser whose output is the result of applying the function to the value
    /// </summary>
    let applyP fP xP =
        fP >>= (fun f -> xP >>= (f >> returnP))
    /// <summary>
    /// Takes a parser whose output is a function, and a parser whose output is a value,
    /// and creates a parser whose output is the result of applying the function to the value
    /// </summary>
    let ( <*> ) = applyP
    /// <summary>
    /// Creates a parser from a list of parsers, that succeeds on input if
    /// any of the parsers from the list would succeed on the input
    /// </summary>
    let choice ps = List.reduce orElse ps
    /// <summary>
    /// Creates a parser from a funtion f and a parser p, where the output is
    /// the result of mapping the output of p with the function f
    /// </summary>
    let mapP f = bindP (f >> returnP)
    /// <summary>
    /// Creates a parser from a funtion f and a parser p, where the output is
    /// the result of mapping the output of p with the function f
    /// </summary>
    let (|>>) p f = mapP f p
    /// <summary>
    /// Tries to let the parser p parse as much input as possible
    /// </summary>
    let parseZeroOrMore p input = 
        let rec aux input acc =
            match run p input with
            | Failure pe -> Success (List.rev acc, input)
            | Success (v, remainingInput) ->
                aux remainingInput (v::acc)

        aux input []
    /// <summary>
    /// Creates a parser that match as much input as possible, and allows for no matches
    /// </summary>
    let many p = mkParser (parseZeroOrMore p) "unknown"
    /// <summary>
    /// Creates a parser that match as much input as possible, but needs at least one match
    /// </summary>
    let many1 p = 
        p >>= fun x -> many p >>= fun xs -> returnP (x :: xs)
    /// <summary>
    /// Takes a list of parsers, and creates a single parser whose output is the output of all
    /// parsers in the list
    /// </summary>
    let rec sequence =
        function
        | []      -> returnP []
        | x :: xs -> returnP (fun h tl -> h :: tl) <*> x <*> (sequence xs)
    /// <summary>
    /// Takes a parser and converts it into a parser that does not fail, but instead output an option
    /// </summary>
    let opt p = p |>> Some <|> returnP None
    /// <summary>
    /// Creates a parser by binding p2 to p1, but only outputs the output of p1
    /// </summary>
    let (.>>) p1 p2 = p1 .>>. p2 |>> fst
    /// <summary>
    /// Creates a parser by binding p2 to p1, but only outputs the output of p2
    /// </summary>
    let (>>.) p1 p2 = p1 .>>. p2 |>> snd
    /// <summary>
    /// Creates a parser by binding p3 to p2, and p2 to p1, but only outputs the output of p2
    /// </summary>
    let between p1 p2 p3 = p1 >>. p2 .>> p3
    /// <summary>
    /// Creates a parser that match on the same input as p at least once, followed by
    /// 0 or more matches from p preceded by matches from sep.
    /// </summary>
    let sepBy1 p sep =
        p .>>. many (sep >>. p)
        |>> fun (x,xs) -> x::xs
    /// <summary>
    /// Creates a parser that matches on the input of p seperated by sep, 0 or more times
    /// </summary>
    let sepBy p sep =
        sepBy1 p sep <|> returnP []
    /// <summary>
    /// Creates a parser that will run the parser located at the parser ref
    /// </summary>
    let createParserForwardedToRef<'a, 'b> () =

        let dummyParser= 
            let innerFn input : Result<'b * 'a, ParseError<'a>> = failwith "unfixed forwarded parser"
            mkParser innerFn "unknown"

        let parserRef = ref dummyParser 

        let innerFn input = run !parserRef input 

        mkParser innerFn "unknown", parserRef
        
module TextParser =

    open Parser
    /// <summary>
    /// Creates a string from a char list
    /// </summary>
    let charListToStr charList = String(List.toArray charList) |> string
    /// <summary>
    /// Creates a parser that matches on a specific char c
    /// </summary>
    let pchar c =
        satisfy TextInputState.nextChar (fun x -> x = c) (sprintf "%c" c)
    /// <summary>
    /// A parser that matches on any letter
    /// </summary>
    let letterChar =
        satisfy TextInputState.nextChar Char.IsLetter "letter"
    /// <summary>
    /// Creates a parser that matches on any letter in the specified list
    /// </summary>
    let anyOf ss =
        ss |> List.map pchar |> Parser.choice <?> 
        sprintf "anyOf %A" ss
    /// <summary>
    /// Creates a parser that outputs a string created by character outputs
    /// </summary>
    let manyChars p  = many p  |>> charListToStr
    /// <summary>
    /// Creates a parser that outputs a string created by character outputs, with at least one match
    /// </summary>
    let manyChars1 p = many1 p |>> charListToStr
    /// <summary>
    /// Creates a parser that matches on the exact string provided
    /// </summary>
    let pstring (str:string) =
        str |> List.ofSeq |> List.map pchar |> sequence |>> charListToStr <?> str


    /// <summary>
    /// A parser that matches on a whitespace
    /// </summary>
    let whitespaceChar =
        satisfy TextInputState.nextChar (Char.IsWhiteSpace) "whitespace"
    /// <summary>
    /// A parser that matches on any amount of whitespaces
    /// </summary>
    let spaces = many whitespaceChar
    /// <summary>
    /// A parser that matches on at least one whitespace
    /// </summary>
    let space1 = many1 whitespaceChar
    /// <summary>
    /// A parser that matches on a digit
    /// </summary>
    let digitChar =
        satisfy TextInputState.nextChar Char.IsDigit "digit"
    /// <summary>
    /// A parser that matches on at least one digit
    /// </summary>
    let digits = manyChars1 digitChar
    /// <summary>
    /// A parser that matches on a letter or a digit
    /// </summary>
    let alphaNumeric = letterChar <|> digitChar
    /// <summary>
    /// A parser that parses an integer
    /// </summary>
    let pint =
        opt (pchar '-') .>>. digits |>>
        function
        | (Some _, ds) -> -(int ds)
        | (None, ds)   -> int ds
        
    /// <summary>
    /// Given a parser and a string, returns the result of running the parser with the string as input
    /// </summary>
    let runTextParser parser inputStr =
        run parser (TextInputState.initialInputState inputStr)
    /// <summary>
    /// Given a parser and a filepath, returns the result of running the parser with the file as input
    /// </summary>
    let runParserFromFile parser path =
        runTextParser parser (System.IO.File.ReadAllText path)