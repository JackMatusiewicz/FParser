module FParser.Parser

type Result<'T> =
    | Success of 'T
    | Failure of string

type Parser<'T> = Parser of (char list -> Result<'T * char list>)

let charParser (c : char) : Parser<char> =
    let parser (input : char list) =
        match input with
        | [] -> Failure "No input"
        | hd :: tl ->
            match hd with
            | _ when hd = c ->
                Success (hd, tl)
            | _ -> Failure (sprintf "Expected %c but got %c" c hd)
    Parser parser

let execute parser =
    let (Parser p) = parser
    fun (input : char list) -> p input

let andThen (a : Parser<'T>) (b : Parser<'U>) =
    let innerAndThen (input : char list) =
        let resultOfFirst = (execute a input)
        match resultOfFirst with
        | Failure f -> Failure f
        | Success (token, rest) ->
            let resultOfSecond = (execute b rest)
            match resultOfSecond with
            | Failure f -> Failure f
            | Success(secondToken, rest) ->
                Success ((token, secondToken), rest)
    (Parser innerAndThen)

let orElse (a : Parser<'T>) (b : Parser<'T>) =
    let parse (input : char list) =
        let firstParserResult = (execute a input)
        match firstParserResult with
        | Success r -> Success r
        | Failure _ -> (execute b input)
    (Parser parse)

let pick = List.reduce orElse

let any (characters : char list) =
    (characters |> List.map charParser) |> pick