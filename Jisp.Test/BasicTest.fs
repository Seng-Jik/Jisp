module Jisp.Test.BasicTest

open NUnit.Framework
open Jisp.Parser.Basic
open OurParserC.Parser

let test parser input =
    OurParserC.Input.create input
    |> parser
    |> printfn "%A"

[<Test>]
let Whitespaces () =
    test whitespace0 "   12345 "
    test whitespace0 "12345"
    test whitespace1 "12345"
    test whitespace1 "   12345"

[<Test>]
let Lambda () =
    test lambda "λfunc"
    test lambda "func"

[<Test>]
let Brackets () =
    test openBracket "( 1"
    test openBracket ")"
    test closeBracket ") 1"
    test closeBracket "("

[<Test>]
let Alpha () =
    test alpha "a"
    test alpha "1"
    test alpha "λ"

[<Test>]
let Number () =
    test number "1"
    test number "a"
    test number "λ"

[<Test>]
let Punctuation () =
    test punctuation "+"
    test punctuation "1"
    test punctuation "λ"

[<Test>]
let Identifier () =
    test (oneOrMore (identifier <@+> whitespace1)) "super +123 -sucker )"

(*[<Test>]
let NumberJispValue () =
    test numberJispValue "12345"
    test numberJispValue "-12345"
    test numberJispValue "\'s\'"

[<Test>]
let StringJispValue () =
    test stringJispValue "\"123456\""
    test stringJispValue "\"\""
    test stringJispValue "\""*)