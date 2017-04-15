module FParser.Operators
open FParser.Parser

let (.>>.) = andThen
let (<||>) = orElse
