module Jisp.RuntimeLibrary

open AST
open Evalution


let getNumber : JispValue -> Result<JispNumber,exn> = function
| Number x -> Ok x
| _ -> Error InvalidArguments
    
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
        | _ -> Error InvalidArguments)
    rtFunc booleanFunction
        
let ifExpression : RuntimeFunc = fun context ->
    function
    | condition::yes::no::[] ->
        match eval context condition with
        | Ok (Number x) when x <> 0M -> eval context yes
        | Ok (Number x) when x = 0M -> eval context no
        | _ -> Error InvalidArguments
    | _ -> Error InvalidArguments

let getStr x = 
    x
    |> List.map (function
    | Number a -> char a
    | _ -> raise InvalidArguments)
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
                | _ -> Error InvalidArguments)
        with e -> Error e
    | _ -> Error InvalidArguments


let isEmpty : RuntimeFunc = fun context ->
    function
    | expr::[] ->
        eval context expr
        |> Result.bind (function
        | Tuple [] -> Ok (Number 1M)
        | Tuple _ -> Ok (Number 0M)
        | _ -> Error InvalidArguments)
    | _ -> Error InvalidArguments

let cons : RuntimeFunc = fun context ->
    function
    | expr::tuple::[] ->
        eval context expr
        |> Result.bind (fun v ->
            eval context tuple
            |> Result.bind (fun ls -> 
                match ls with
                | Tuple x -> Ok (Tuple (v::x))
                | _ -> Error InvalidArguments))
    | _ -> Error InvalidArguments

let head : RuntimeFunc = fun context ->
    function
    | tuple::[] ->
        eval context tuple
        |> Result.bind (function
        | Tuple (x::_) -> Ok x
        | _ -> Error InvalidArguments)
    | _ -> Error InvalidArguments

let tail : RuntimeFunc = fun context ->
    function
    | tuple::[] ->
        eval context tuple
        |> Result.bind (function
        | Tuple (_::x) -> Ok (Tuple x)
        | Tuple [] -> Ok (Tuple [])
        | _ -> Error InvalidArguments)
    | _ -> Error InvalidArguments

exception Failwith of string
let jispFailwith : RuntimeFunc = fun context ->
    function
    | str::[] ->
        try
            eval context str
            |> Result.bind(function
            | Tuple x -> 
                Error (Failwith (getStr x))
            | _ -> Error InvalidArguments)
        with e -> Error e
    | _ -> Error InvalidArguments

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
            | _ -> Error InvalidArguments)
        with e -> Error e
    | _ -> Error InvalidArguments

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
        | _ -> Error InvalidArguments
    | _ -> Error InvalidArguments)

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
            | _ -> Error InvalidArguments
        with e -> Error e
    | _ -> Error InvalidArguments)

let jispTuple : RuntimeFunc = fun context ->
    evalParams context
    >> Result.map Tuple

                
let defaultContext : Context = {
    Local = Map.empty
    Global = 
    [
        "$bind",evalJispString "(λ value f (f value))"
        "?", rtFunc ifExpression

        "is-empty", rtFunc isEmpty
        "invoke",rtFunc jispInvoke
        "eval", rtFunc jispEval
        "failwith", rtFunc jispFailwith
        "tuple", rtFunc jispTuple
        "cons", rtFunc cons
        "head", rtFunc head
        "tail", rtFunc tail

        "read-file", rtFunc jispReadFile
        "print-str-ln", rtFunc printStrLn

        "+", arithmeticOperator (+)
        "-", arithmeticOperator (-)
        "*", arithmeticOperator (*)
        "/", arithmeticOperator (/)
        "=", comparisonOperator (=)
        "<", comparisonOperator (<) ]
    |> bindValues Map.empty
}
    (* 需要完成后将一些内置函数移动到标准库中
    • len 求list长度
    • concat 连接两个list
    • choose
    • collect
    • map
    • fold
    • filter
    • reduce
    • unfold
    • generate
    • Y
    * call-cc（可以用异常来实现）
    * try-catch *)