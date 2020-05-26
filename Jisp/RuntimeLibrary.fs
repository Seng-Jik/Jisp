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
    >> Result.bind (fst >> eval { Global = Map.empty; Local = Map.empty })
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
        
let comparisonOperator func =
    let booleanFunction : RuntimeFunc = fun context ->
        evalParams context
        >> Result.bind (function
        | (Number a)::(Number b)::[] -> (if (func a b) then 1M else 0M) |> Number |> Ok
        | _ -> Error (InvalidArguments "For comparision operator, only pass 2 arguments."))
    rtFunc booleanFunction
        
let ifExpression : RuntimeFunc = fun context ->
    function
    | condition::yes::no::[] ->
        match eval context condition with
        | Ok (Number x) when x <> 0M -> eval context yes
        | Ok (Number x) when x = 0M -> eval context no
        | _ -> Error (InvalidArguments "For ? function, the first argument must be bool.")
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
        | Tuple [] -> Ok (Number 1M)
        | Tuple _ -> Ok (Number 0M)
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

let jispInvoke : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | func::argPack::[] -> 
        match func,argPack with
        | Lambda func,Tuple args ->
            Apply {
                Function = Value (Lambda func)
                Arguments = args |> List.map Value
            }
            |> eval (match func with
                    | RuntimeFunc -> context
                    | CustumFunc x -> {context with Local = x.FunctionContext})
        | _ -> Error (InvalidArguments "For invoke function, the arguments should be function and a tuple.")
    | _ -> Error (InvalidArguments "For read-file function, only pass 2 arguments."))

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

exception CallCCException of JispValue
let jispCallCC : RuntimeFunc = fun context ->
    evalParams context
    >> Result.bind (function
    | Lambda f::[] -> 
        Apply {
            Function = Value (Lambda f)
            Arguments = 
                [
                    Value (rtFunc (fun context -> 
                        evalParams context
                        >> Result.bind (function
                        | a::[] -> CallCCException a |> Error
                        | _ -> InvalidArguments "For continuation, only pass one argument." |> Error))) ]
        }
        |> eval context
        |> function
        | Ok result | Error (CallCCException result) -> Ok result
        | Error e -> Error e
    | _ -> Error (InvalidArguments "For call-cc function, only pass 1 function as argument."))

                
let defaultContext : Context = {
    Local = Map.empty
    Global = 
    [
        "$bind",evalJispString "(λ value f (f value))"
        "?", rtFunc ifExpression

        "is-empty", rtFunc isEmpty
        "invoke",rtFunc jispInvoke
        "Y", rtFunc jispY
        "call-cc", rtFunc jispCallCC
        "eval", rtFunc jispEval
        "failwith", rtFunc jispFailwith
        "tuple", rtFunc jispTuple
        "head", rtFunc head
        "tail", rtFunc tail
        "concat", rtFunc jispConcat

        "read-file", rtFunc jispReadFile
        "print-str", rtFunc printStr
        "print-str-ln", rtFunc printStrLn

        "+", arithmeticOperator (+)
        "-", arithmeticOperator (-)
        "*", arithmeticOperator (*)
        "/", arithmeticOperator (/)
        "%", arithmeticOperator (%)
        "=", comparisonOperator (=)
        "<", comparisonOperator (<) ]
    |> bindValues Map.empty
}
(* 使用call-cc实现控制流
    * for-each
    * for
    * while
    * do
    * return 
    * try-catch
    *)