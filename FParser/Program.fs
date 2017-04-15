// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

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
            | _ -> Failure (sprintf "Expectd %c but got %c" c hd)
    Parser parser

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code

