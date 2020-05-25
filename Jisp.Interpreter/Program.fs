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
        while true do
            Console.ForegroundColor <- ConsoleColor.Green
            printf "> "
            let line = Console.ReadLine ()

            Console.ForegroundColor <- ConsoleColor.Cyan
            line
            |> Jisp.Parser.Parser.parse
            |> function
            | Error (e,_) ->
                Console.ForegroundColor <- ConsoleColor.Red
                printfn "Synax Error:%A" e
            | Ok (e,_) -> Jisp.Evalution.run Jisp.RuntimeLibrary.defaultContext e
            printfn ""
        Console.ForegroundColor <- ConsoleColor.Gray
        0  

    | "-c"::src::dest::[] ->  // Run as compiler
        0 // Jisp 编译器

    | filename::args ->   // Run as Interpreter
        System.IO.File.ReadAllText filename
        |> Jisp.Parser.Parser.parse
        |> function
        | Error (e,_) -> 
            printfn "Synax Error:%A" e
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
            0
