module Jisp.Test.PreprocessorTest

open NUnit.Framework
open Jisp.Parser.Preprocessor

[<Test>]
let Preprocess () =
    """
        ($test 1) ;100

        ($test 2) ; 200  

        (+ test1 test2)
        """
    |> preprocess
    |> printfn "%A"

