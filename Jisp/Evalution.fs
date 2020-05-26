module rec Jisp.Evalution

open AST

exception IdenifierNotFound of string
exception CanNotCallTheValue
exception InvalidArguments
exception Exit of JispValue

let bindValues (binds:Map<string,JispValue>) nameValueSeq = 
    Seq.fold (fun local (name,value) -> Map.add name value local) binds nameValueSeq


let getValueFromContext (context:Context) (id:string) : Result<JispValue,exn> =
    match Map.tryFind id context.Local with
    | Some x -> Ok x
    | None ->
        match Map.tryFind id context.Global with
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
            { x with
                FunctionContext = context.Local }
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
            | RuntimeFunc func -> func context param
            | CustumFunc func -> 
                evalParams context param
                |> Result.bind (fun arguments -> 
                    let argCount = List.length arguments
                    let paramCount = List.length func.Parameters
                    if argCount > paramCount then
                        Error InvalidArguments
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
                                eval { context with Local = f.FunctionContext } f.Expression))
       
       
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


let run context =
    eval context
    >> function
    | Error (Exit result) ->
        printResult result
        Error (Exit result)
    | Ok result -> 
        printResult result
        Ok (result)
    | Error e -> 
        printfn "Error:%A" e
        Error e

        
        