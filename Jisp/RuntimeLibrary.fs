module Jisp.RuntimeLibrary

open AST
open Evalution


let getNumber : JispValue -> Result<JispNumber,exn> = function
| Number x -> Ok x
| _ -> Error (InvalidArguments "This argument shoud be number.")
    
let rtFunc = RuntimeFunc >> Lambda

let evalJispString =
    OurParserC.Input.create
    >> Jisp.Parser.Expression.expression
    >> Result.mapError fst
    >> Result.bind (fst >> eval { Local = Map.empty; Level = 0UL })
    >> function
    | Ok v -> v
    | Error e -> raise e
    

let arithmeticOperator func =
    let numberFunction : RuntimeFunc = fun context ->
        evalParams context
        >> Result.bind (
            List.map getNumber
            >> List.reduce (fun (x:Result<JispNumber,exn>) (y:Result<JispNumber,exn>) -> 
                match x with
                | Ok x -> y |> Result.bind (fun y -> Ok (func x y))
                | Error x -> Error x))
        >> Result.map Number
    rtFunc numberFunction

let trueFunction = evalJispString "(λ a b (a))"
let falseFunction = evalJispString "(λ a b (b))"
        
let comparisonOperator func =
    let booleanFunction : RuntimeFunc = fun context ->
        evalParams context
        >> Result.bind (function
        | (Number a)::(Number b)::[] -> (if (func a b) then trueFunction else falseFunction) |> Ok
        | _ -> Error (InvalidArguments "For comparision operator, only pass 2 arguments."))
    rtFunc booleanFunction
        
let ifExpression : RuntimeFunc = fun context ->
    function
    | condition::yes::no::[] ->
        match eval context condition with
        | Ok (Lambda boolFunc) ->
            let ast = Apply { Function = Value (Lambda boolFunc); Arguments = [Value (Number 1.0M); Value (Number 0.0M)] }
            match eval context ast with
            | Ok (Number 1.0M) -> eval context yes
            | Ok (Number 0.0M) -> eval context no
            | Ok _ -> Error (InvalidArguments "For ? function, the first argument must be bool function.")
            | Error e -> Error e
        | Ok _ -> Error (InvalidArguments "For ? function, the first argument must be bool function.")
        | Error e -> Error e
    | _ -> Error (InvalidArguments "For ? function, only pass 3 arguments.")

let getStr x = 
    x
    |> List.map (function
    | Number a -> char a
    | _ -> raise (InvalidArguments "This argument should be string."))
    |> List.toArray
    |> fun x -> new System.String (x)

let printStrLn : RuntimeFunc = fun context ->
    function
    | str::[] ->
        try
            eval context str
            |> Result.bind (fun x ->
                match x with
                | Tuple x ->
                    getStr x |> printfn "%s"
                    Ok (Tuple [])
                | Number x ->
                    printfn "%s" (string (char x))
                    Ok (Tuple [])
                | _ -> Error (InvalidArguments "For print-str-ln function, the argument should be number or tuple."))
        with e -> Error e
    | _ -> Error (InvalidArguments "For print-str-ln function, only pass 1 argument.")

let printStr : RuntimeFunc = fun context ->
    function
    | str::[] ->
        try
            eval context str
            |> Result.bind (fun x ->
                match x with
                | Tuple x ->
                    getStr x |> printf "%s"
                    Ok (Tuple [])
                | Number x ->
                    printf "%s" (string (char x))
                    Ok (Tuple [])
                | _ -> Error (InvalidArguments "For print-str function, the argument should be number or tuple."))
        with e -> Error e
    | _ -> Error (InvalidArguments "For print-str function, only pass 1 argument.")


let isEmpty : RuntimeFunc = fun context ->
    function
    | expr::[] ->
        eval context expr
        |> Result.bind (function
        | Tuple [] -> Ok trueFunction
        | Tuple _ -> Ok falseFunction
        | _ -> Error (InvalidArguments "For is-empty function, the argument should be tuple."))
    | _ -> Error (InvalidArguments "For is-empty function, only pass 1 argument.")

let head : RuntimeFunc = fun context ->
    function
    | tuple::[] ->
        eval context tuple
        |> Result.bind (function
        | Tuple (x::_) -> Ok x
        | _ -> Error (InvalidArguments "For head function, the argument should be tuple."))
    | _ -> Error (InvalidArguments "For head function, only pass 1 argument.")

let tail : RuntimeFunc = fun context ->
    function
    | tuple::[] ->
        eval context tuple
        |> Result.bind (function
        | Tuple (_::x) -> Ok (Tuple x)
        | Tuple [] -> Ok (Tuple [])
        | _ -> Error (InvalidArguments "For tail function, the argument should be tuple."))
    | _ -> Error (InvalidArguments "For tail function, only pass 1 argument.")

exception Failwith of string
let jispFailwith : RuntimeFunc = fun context ->
    function
    | str::[] ->
        try
            eval context str
            |> Result.bind(function
            | Tuple x -> 
                Error (Failwith (getStr x))
            | _ -> Error (InvalidArguments "For exit function, the argument should be string."))
        with e -> Error e
    | _ -> Error (InvalidArguments "For exit function, only pass 1 argument.")

exception CanNotOpenFile of string
let jispReadFile : RuntimeFunc = fun context ->
    function
    | path::[] ->
        try
            eval context path
            |> Result.bind (function
            | Tuple x ->
                let fileName = getStr x
                try
                    System.IO.File.ReadAllBytes fileName
                    |> Array.map (int >> JispNumber >> Number)
                    |> Array.toList
                    |> Tuple
                    |> Ok
                with _ -> Error (CanNotOpenFile fileName)
            | _ -> Error (InvalidArguments "For read-file function, the argument should be string."))
        with e -> Error e
    | _ -> Error (InvalidArguments "For read-file function, only pass 1 argument.")

let jispEval : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | str::[] -> 
        try
            match str with
            | Tuple str ->
                let code = getStr str
                code
                |> Jisp.Parser.Parser.parse
                |> function
                | Error (e,_) -> Error e
                | Ok (ast,_) ->
                    eval context ast
            | _ -> Error (InvalidArguments "For invoke function, the argument should be string.")
        with e -> Error e
    | _ -> Error (InvalidArguments "For eval function, only pass 1 argument."))

let jispTuple : RuntimeFunc = fun context ->
    evalParams context
    >> Result.map Tuple

let jispY : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | (Lambda f)::[] ->
        let rec callSelf context : JispExpr list -> Result<JispValue,exn> =
            evalParams context
            >> Result.bind (fun x ->
                Apply {
                    Function = Value (Lambda f)
                    Arguments = Value (rtFunc callSelf) :: (x |> List.map Value)
                }
                |> eval context)
        Ok (rtFunc callSelf)
    | _ -> Error (InvalidArguments "For Y combinator, only pass a function."))

let jispConcat : RuntimeFunc = fun context a ->
    try
        evalParams context a
        |> Result.bind (
            List.map (function
            | (Tuple x) -> x
            | _ -> raise (InvalidArguments "For concat function, only pass tuples as arguments."))
            >> List.concat
            >> Tuple
            >> Ok)
    with e -> Error e

exception CallCCException of JispValue*Level:uint64
let jispCallCC : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | Lambda f::[] ->
        let level = context.Level
        Apply {
            Function = Value (Lambda f)
            Arguments = 
                [
                    Value (rtFunc (fun context -> 
                        evalParams context
                        >> Result.bind (function
                        | a::[] -> CallCCException (a,level) |> Error
                        | _ -> InvalidArguments "For continuation, only pass one argument." |> Error))) ]
        }
        |> eval context
        |> function
        | Error (CallCCException (result,level2)) when level2 = level -> Ok result
        | Ok result -> Ok result
        | Error e -> Error e
    | _ -> Error (InvalidArguments "For call-cc function, only pass 1 function as argument."))

let jispPrint : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | x :: [] -> 
        printResult x
        Ok (Tuple [])
    | _ -> Error (InvalidArguments "For print function, only pass 1 argument."))

let jispPrintLn : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | x :: [] -> 
        printResult x
        printfn ""
        Ok (Tuple [])
    | _ -> Error (InvalidArguments "For print function, only pass 1 argument."))

let jispReadline : RuntimeFunc = fun _ _ ->
    System.Console.ReadLine().ToCharArray()
    |> Array.toList
    |> List.map (int >> JispNumber >> Number)
    |> Tuple
    |> Ok

let jispCreateDirectory : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | Tuple name :: [] ->
        name
        |> List.map (function
        | Number n -> char n
        | _ -> failwith "?")
        |> List.toArray
        |> fun x -> new System.String (x)
        |> System.IO.Directory.CreateDirectory
        |> fun _ -> Tuple []
        |> Ok
    | _ -> Error (InvalidArguments ""))

let private evalStr x =
    x
    |> List.map (function
    | Number n -> char n
    | _ -> failwith "?")
    |> List.toArray
    |> fun x -> new System.String (x)

let jispWriteTextFile : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | Tuple content :: Tuple name :: [] ->
        System.IO.File.WriteAllText (evalStr name, evalStr content)
        |> fun _ -> Tuple []
        |> Ok
    | _ -> Error (InvalidArguments "jispWriteTextFile"))

let jispReadTextFile : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | Tuple name::[] ->
        evalStr name
        |> System.IO.File.ReadAllText
        |> Seq.map (int >> decimal >> Number)
        |> Seq.toList |> Tuple |> Ok
    | _ -> Error (InvalidArguments "jispReadTextFile"))

let jispReadKey : RuntimeFunc = fun _ _ ->
    System.Console.ReadKey (true)
    |> fun x -> x.KeyChar
    |> (int >> JispNumber >> Number)
    |> Ok

let jispSystemRun : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | Tuple cmd:: args ->
        let cmd = evalStr cmd
        let args = 
            args 
            |> List.map (function
            | Tuple n -> evalStr n
            | _ -> failwith "jispRun failed.")
        let args = 
            match args with
            | [] -> ""
            | args -> Seq.reduce (fun a b -> a + " " + b) args
        
        let startInfo = new System.Diagnostics.ProcessStartInfo ()
        startInfo.FileName <- cmd
        startInfo.Arguments <- args
        startInfo.UseShellExecute <- false
        startInfo.WorkingDirectory <- System.Environment.CurrentDirectory
        
        let prc = System.Diagnostics.Process.Start(startInfo)
        prc.WaitForExit ()
        Ok (Tuple [])
    | _ -> Error (InvalidArguments "jispRun failed."))

let jispDelete : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function 
    | Tuple toDel::[] ->
        let toDel = evalStr toDel
        try System.IO.File.Delete toDel with _ -> ()
        try System.IO.Directory.Delete (toDel, true) with _ -> ()
        Ok <| Tuple []
    | _ -> Error <| InvalidArguments "jispDelete")

let defaultContext : Context = {
    Level = 0UL
    Local = 
    [
        "bind",evalJispString "(λ f value (f value))"
        "?", rtFunc ifExpression

        "is-empty", rtFunc isEmpty
        "Y", rtFunc jispY
        "call-cc", rtFunc jispCallCC
        "eval", rtFunc jispEval
        "failwith", rtFunc jispFailwith
        "tuple", rtFunc jispTuple
        "head", rtFunc head
        "tail", rtFunc tail
        "concat", rtFunc jispConcat

        "read-key", rtFunc jispReadKey
        "read-line", rtFunc jispReadline
        "print", rtFunc jispPrint
        "print-ln", rtFunc jispPrintLn
        "print-str", rtFunc printStr
        "print-str-ln", rtFunc printStrLn

        "system", rtFunc jispSystemRun
        "delete", rtFunc jispDelete
        "read-file", rtFunc jispReadFile
        "read-text-file", rtFunc jispReadTextFile
        "create-directory", rtFunc jispCreateDirectory
        "write-text-file", rtFunc jispWriteTextFile

        "+", arithmeticOperator (+)
        "-", arithmeticOperator (-)
        "*", arithmeticOperator (*)
        "/", arithmeticOperator (/)
        "%", arithmeticOperator (%)
        "=", comparisonOperator (=)
        "<", comparisonOperator (<) ]
    |> bindValues Map.empty
}
