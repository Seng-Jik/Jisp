module Jisp.Test.StdlibTest

open NUnit.Framework
open Jisp.AST


let test expect =
    Jisp.Parser.Parser.parse
    >> function
    | Error (e,_) -> raise e
    | Ok (ast,_) ->
        Jisp.Evalution.eval Jisp.RuntimeLibrary.defaultContext ast
        |> function
        | Error e -> raise e
        | Ok value ->
            match value with
            | Number value ->
                if expect = value |> not then
                    failwith (sprintf "%A %A" expect value)
            | _ -> failwith ""

let run =
    Jisp.Parser.Parser.parse
    >> function
    | Error (e,_) -> raise e
    | Ok (ast,_) ->
        Jisp.Evalution.eval Jisp.RuntimeLibrary.defaultContext ast
        |> function
        | Error e -> raise e
        | Ok x -> 
            Jisp.Evalution.printResult x

[<Test>]
let Basics () =
    test 1M "(is-empty (ignore 123))"
    test 1M "(invoke (> 2) (cons 1 ()))"
    test 0M "(invoke (< 2)) (cons 1 ())"
    test 3M """(eval "+ 1 2")"""

    let rec fibo = function
    | 1 | 2 -> 1M
    | x -> fibo (x - 1) + fibo (x - 2)

    let jispFibo = """
        ($fibo (Y (λ self n 
            (? (| (= n 1) (= n 2)) 
                1
                (+ (self (- n 1)) (self (- n 2)))))))
        """

    for i in 1..10 do
        test (fibo i) (jispFibo + sprintf "(fibo %d)" i) 
        run (jispFibo + sprintf "(fibo %d)" i) 

    test 1M """
    ($fibo (Y (λ self n 
        (? (| (= n 1) (= n 2)) 
            1
            (+ (self (- n 1)) (self (- n 2)))))))

    (fibo 2)

    """

[<Test>]
let Booleans () =
    test 1M "true"
    test 0M "(false)"
    test 100M "(boolean-function true) 100 200"
    test 200M "(boolean-function false) 100 200"

[<Test>]
let BooleanOperators () =
    test 1M "| true false"
    test 1M "| true true"
    test 1M "| false true"
    test 0M "(| false false)"
    test 1M "& true true"
    test 0M "& false true"
    test 0M "& true false"
    test 0M "& false false"
    test 1M "! false"
    test 0M "! true"

[<Test>]
let FunctionOperators () =
    test 0M "(>> (| true) (& false)) true"
    test 1M "(>> (| true) (& true)) false"
    test 1M "(<< (| true) (& true)) false"
    test 1M "(<< (| true) (& true)) false"

[<Test>]
let ArithmeticOperators () =
    test (1M+2M+3M+4M+5M) "+ 1 2 3 4 5"
    test (1M-2M-3M-4M-5M) "- 1 2 3 4 5"
    test (1M*2M*3M*4M*5M) "* 1 2 3 4 5"
    test (1M/2M/3M/4M/5M) "/ 1 2 3 4 5"

[<Test>]
let ComparisonOperators () =
    test 1M "= 1 1"
    test 0M "= 1 2"
    test 1M "!= 1 2"
    test 0M "!= 2 2"

    test 0M "< 2 1"
    test 0M "< 2 2"
    test 1M "< 1 2"
    test 0M "<= 2 1"
    test 1M "<= 2 2"
    test 1M "<= 1 2"

    test 1M "> 2 1"
    test 0M "> 2 2"
    test 0M "> 1 2"
    test 1M ">= 2 1"
    test 1M ">= 2 2"
    test 0M ">= 1 2"

    test 1M "(= 1) 1"
    test 0M "(= 1) 2"
    test 1M "(!= 1) 2"
    test 0M "(!= 2) 2"

    test 0M "(< 2) 1"
    test 0M "(< 2) 2"
    test 1M "(< 1) 2"
    test 0M "(<= 2) 1"
    test 1M "(<= 2) 2"
    test 1M "(<= 1) 2"

    test 1M "(> 2) 1"
    test 0M "(> 2) 2"
    test 0M "(> 1) 2"
    test 1M "(>= 2) 1"
    test 1M "(>= 2) 2"
    test 0M "(>= 1) 2"

[<Test>]
let Tuple () =
    run "print-str-ln (cons 'H' (cons 'i' ()))"
    test 2M "head (tail (cons 1 (cons 2 (cons 3 ()))))"
    run "print-str-ln (tuple 'H' 'e' 'l' 'l' 'o' ' ' 'T' 'u' 'p' 'l' 'e')"
    run """print-str-ln (concat "Hello" "World" "!")"""
    test 2M "len (tuple 1 2)"

[<Test>]
let IO () =
    run """print-str-ln"""
    run """print-str-ln "Hello, world!" """

    let path = System.AppDomain.CurrentDomain.SetupInformation.ApplicationBase + "stdlib.jisp"
    test (decimal (int ';')) (sprintf "head (read-file \"%s\")" path)