﻿module rec Jisp.Evalution

open AST

exception IdenifierNotFound of string
exception CanNotCallTheValue
exception InvalidArguments of string

let bindValues (binds:Map<string,JispValue>) nameValueSeq = 
    Seq.fold (fun local (name,value) -> Map.add name value local) binds nameValueSeq


let getValueFromContext (context:Context) (id:string) : Result<JispValue,exn> =
    match Map.tryFind id context.Local with
    | Some x -> Ok x
    | None -> Error (IdenifierNotFound id)
    
let evalParams context : JispExpr list -> Result<JispValue list,exn> = function
| a :: tail -> 
    eval context a 
    |> Result.bind (fun a ->
        evalParams context tail 
        |> Result.bind (fun t -> 
            Ok (a::t)))
| [] -> Ok []

let eval (context:Context) (ast:JispExpr) : Result<JispValue,exn> =
    match ast with
    | Value v -> 
        match v with
        | Lambda (CustumFunc x) ->
            if Map.isEmpty x.FunctionContext then
                { x with
                    FunctionContext = context.Local }
            else x
            |> CustumFunc 
            |> Lambda
        | x -> x
        |> Ok
    | Identifier id -> getValueFromContext context id
    | Apply { Function = f; Arguments = param } -> 
        eval context f
        |> Result.bind (fun target ->
            match target with
            | Lambda x -> Ok x
            | _ -> Error CanNotCallTheValue)
        |> Result.bind (fun f -> 
            match f with
            | RuntimeFunc func -> func { context with Level = context.Level + 1UL } param
            | CustumFunc func -> 
                evalParams context param
                |> Result.bind (fun arguments -> 
                    let argCount = List.length arguments
                    let paramCount = List.length func.Parameters
                    if argCount > paramCount then
                        Apply {
                            Function = 
                                Apply {
                                    Function = Value <| Lambda f
                                    Arguments = arguments |> List.take paramCount |> List.map Value
                                }
                            Arguments = arguments |> List.skip paramCount |> List.map Value
                        }
                        |> eval context
                    else
                        { func with
                            Parameters = List.skip argCount func.Parameters
                            FunctionContext = 
                                bindValues 
                                    func.FunctionContext
                                    (List.zip (List.take argCount func.Parameters) arguments) }
                        |> fun f ->
                            if (List.length f.Parameters) > 0 then
                                f |> CustumFunc |> Lambda |> Ok
                            else
                                eval { 
                                    context with 
                                        Local = f.FunctionContext
                                        Level = context.Level + 1UL } f.Expression))
       
       
let rec printResult = function
| Number x -> printf "%M" x
| Tuple [] -> ()
| Tuple x -> 
    printf "(tuple"
    List.iter (fun x ->
        printf " "
        printResult x)
        x
    printf ")"
| Lambda x -> 
    match x with
    | RuntimeFunc f -> printf "(λ ??? (%A))" f
    | CustumFunc f ->
        printf "(λ "
        for i in f.Parameters do printf "%s " i
        printf "(...))"


let run context expr =
    let threadFunction () =
        eval context expr
        |> function
        | Ok result -> 
            printResult result
        | Error e -> 
            printfn "Error:%A" e

    let stackSize = 1024 * 1024 * 256
    let thread = System.Threading.Thread (threadFunction,stackSize)
    thread.Start ()
    thread.Join ()



        
        