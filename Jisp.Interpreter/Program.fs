open System

let banner () =
    Console.ForegroundColor <- ConsoleColor.Green
    printfn "Jisp Programming Language"
    printfn "Version 1.0"
    printfn ""

let help () =
    Console.ForegroundColor <- ConsoleColor.Gray
    printfn "Usage:"
    printfn "    Run as Interactive"
    printfn "        jisp"
    printfn "    Run as Interpreter"
    printfn "        jisp <sourceCodeFile> <commandLineArguments>"
    printfn "    Run as Compiler"
    printfn "        jisp -c <sourceCodeFile> <outputFile>"
    printfn "    Display help information"
    printfn "        jisp -?"
    printfn ""


[<EntryPoint>]
let main argv = 
    argv
    |> Array.toList
    |> function
    | "-help"::_ | "-?"::_ | "/?"::_ | "/help"::_ ->
        banner ()
        help ()
        0 

    | [] ->     // Run as Interactive
        banner ()
        printfn "Type \"exit\" to exit interactive."
        printfn ""
        let mutable cont = true
        while cont do
            Console.ForegroundColor <- ConsoleColor.Green
            printf "> "
            let line = Console.ReadLine ()

            if line.Trim () = "exit" then
                cont <- false
            else
                Console.ForegroundColor <- ConsoleColor.Cyan
                line
                |> Jisp.Parser.Parser.parse
                |> function
                | Error (e,_) ->
                    Console.ForegroundColor <- ConsoleColor.Red
                    printfn "Syntax Error:%A" e
                | Ok (e,_) -> 
                    Jisp.Evalution.run Jisp.RuntimeLibrary.defaultContext e |> ignore
                printfn ""
        Console.ForegroundColor <- ConsoleColor.Gray
        0  

    | "-c"::src::dest::[] ->  // Run as compiler
        let src =
            src
            |> System.IO.File.ReadAllText
        src
        |> Jisp.Parser.Parser.parse
        |> function
        | Error (e,t) ->
            printfn "Syntax Error(%d,%d):%A" t.row t.col e
            -1
        | Ok _ ->
            src
            |> Jisp.Parser.Preprocessor.preprocess
            |> fun x ->
                let tail = """

[<EntryPoint>]
let main args =
    src
    |> OurParserC.Input.create
    |> Jisp.Parser.Expression.bareExpression
    |> function
    | Error (e,t) ->
        printfn "Syntax Error(%d,%d):%A" t.row t.col e
        -1
    | Ok (ast,_) ->
        Jisp.Evalution.run 
            { Jisp.RuntimeLibrary.defaultContext with
                Local = 
                    let argv =
                        args
                        |> Array.toList
                        |> List.map (fun (str:string) ->
                            str.ToCharArray ()
                            |> Array.map (int >> decimal >> Jisp.AST.Number)
                            |> Array.toList
                            |> Jisp.AST.Tuple)
                        |> Jisp.AST.Tuple
                    Jisp.Evalution.bindValues 
                        Jisp.RuntimeLibrary.defaultContext.Local
                        ["argv",argv] }
            ast
        |> ignore
        0
            """
                let head = """let src = """
                let code = sprintf "%s\"\"\"\n%s\n\"\"\"%s" head x tail

                let tempSource = Environment.GetEnvironmentVariable("TEMP") + "/temp.fs"

                System.IO.File.WriteAllText (tempSource,code)

                let fscArgs = [
                    "fsc.exe"
                    "-r:Jisp.Core.dll"
                    "-r:OurParserC.dll"

                    "--nologo"
                    "--standalone"
                    "--optimize+"
                    "-o";sprintf "%s" <| System.IO.FileInfo(dest).FullName
                    "--target:exe"
                    "--crossoptimize+"
                    tempSource
                ]

                let checker = FSharp.Compiler.SourceCodeServices.FSharpChecker.Create ()
                checker.Compile (List.toArray fscArgs)
                |> Async.RunSynchronously
                |> fun r -> 
                    let r = fst r
                    r |> Seq.map (fun x -> x.ToString ()) |> Seq.iter (printfn "%s")

                System.IO.File.Delete tempSource

            0

    | filename::args ->   // Run as Interpreter
        System.IO.File.ReadAllText filename
        |> Jisp.Parser.Parser.parse
        |> function
        | Error (e,t) -> 
            printfn "Syntax Error(%d,%d):%A" t.row t.col e
            -1
        | Ok (e,_) -> 
            let argv =
                args
                |> List.map (fun str ->
                    str.ToCharArray ()
                    |> Array.map (int >> decimal >> Jisp.AST.Number)
                    |> Array.toList
                    |> Jisp.AST.Tuple)
                |> Jisp.AST.Tuple
            Jisp.Evalution.run
                { Jisp.RuntimeLibrary.defaultContext with
                    Local = 
                        Jisp.Evalution.bindValues
                            Jisp.RuntimeLibrary.defaultContext.Local ["argv",argv] }
                e
            |> ignore
            0
