module Test
open FParser.Parser
open FParser.Operators
open NUnit.Framework

[<Test>]
let ``Given a parser and a character list when parsed returns success`` () =
    let input = ['a';'b';'c']
    let aParser = charParser 'a'
    let result = execute aParser input
    Assert.That(result, Is.EqualTo(Success ('a', ['b';'c'])))

[<Test>]
let ``Given a parser and a character list that isn't valid when parsed returns failure`` () =
    let input = ['z';'b';'c']
    let aParser = charParser 'a'
    let result = execute aParser input
    Assert.That(result, Is.EqualTo(Failure "Expected a but got z"))

[<Test>]
let ``Given two parsers combines when parsed input then result is correct`` () =
    let aParser = charParser 'a'
    let bParser = charParser 'b'
    let input = ['a';'b';'c';]
    let result = execute (aParser .>>. bParser) input
    Assert.That(result, Is.EqualTo(Success (('a','b'), ['c'])))

[<Test>]
let ``Given two parsers combined when parsed input and fails on second parser then result is correct`` () =
    let aParser = charParser 'a'
    let bParser = charParser 'c'
    let input = ['a';'b';'c';]
    let result = execute (aParser .>>. bParser) input
    Assert.That(result, Is.EqualTo(Failure "Expected c but got b"))

[<Test>]
let ``Given two parsers combined with or else when parsed input and only succeeds on second parser then result is correct`` () =
    let aParser = charParser 'z'
    let bParser = charParser 'a'
    let input = ['a';'b';'c';]
    let result = execute (aParser <||> bParser) input
    Assert.That(result, Is.EqualTo(Success ('a', ['b';'c'])))

[<Test>]
let ``Given digit parser when given input of digits then parsing is successful`` () =
    let digitParser = any ['0' .. '9']
    let input = ['0';'1';'9']
    let result = execute digitParser input
    Assert.That(result, Is.EqualTo(Success ('0', ['1';'9'])))

