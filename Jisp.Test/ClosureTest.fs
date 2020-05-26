module Jisp.Test.ClosureTest

open NUnit.Framework
open Jisp.Parser

let run =
    OurParserC.Input.create
    >> Expression.bareExpression
    >> function
    | Error (e,_) -> raise e
    | Ok (ast,_) ->
        Jisp.Evalution.eval Jisp.RuntimeLibrary.defaultContext ast
        |> function
        | Error e -> raise e
        | Ok x -> 
            Jisp.Evalution.printResult x


[<Test>]
let CustumClosureTest () =
    run "((λ x (λ y (+ x y))) 1) 2"

[<Test>]
let RuntimeClosureTest () =
    run "((λ x (λ y (tuple x y))) 1) 2"

[<Test>]
let BindClosureTest () =
    run """
        ($func (λ f (f 1)))
        ($func2 (λ g (tuple (func g))))
        (func2 (λ x (+ 1 x)))
    """
