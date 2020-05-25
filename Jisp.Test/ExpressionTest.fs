module Jisp.Test.ExpressionTest

open NUnit.Framework
open Jisp.Parser.Expression
open Jisp.Evalution
open Jisp.RuntimeLibrary

let test input =
    input
    |> Jisp.Parser.Preprocessor.preprocess
    |> OurParserC.Input.create
    |> bareExpression
    |> function
    | Error e -> printfn "%A" e
    | Ok (ast,_) ->
        run defaultContext ast

[<Test>]
let TestSimpleExpression () =
    test """(+ 1 2)"""

[<Test>]
let TestBindExpression () =
    """
        ($sucker 1)
        ($sucker2 (+ 1 2))
        (+ sucker sucker 2)
    """
    |> test
    
[<Test>]
let TestLambda () =
    """
        ($add (λ arg1 arg2 arg3 (+ arg1 arg2 arg3)))
        ((add 1 
            (add 1 2 3)) 3)
    """
    |> test

    """(λ a b (+ a b)) 1 2"""
    |> test
