module rec Jisp.Parser.Expression


open OurParserC
open Parser
open Jisp.AST
open Basic

let numberJispValue =
    ((zeroOrOne (Parsers.character '-') <+> oneOrMore number 
        >> Parsed.map (fun (sign,number) -> 
            match sign with
            | Some '-' -> '-' :: number
            | _ -> number) 
        >> charlist2str 
        >> Parsed.map (int >> JispNumber))
    <|> (Parsers.character '\'' <+@> Parsers.anyChar <@+> Parsers.character '\'' 
        >> Parsed.map (int >> JispNumber)))
    >> Parsed.map Number

let stringJispValue =
    Parsers.character '\"' <+@> zeroOrMore (pred Parsers.anyChar ((<>) '\"')) <@+> Parsers.character '\"'
    >> Parsed.map (
        List.map (int >> JispNumber >> Number)
        >> Tuple)

let atomicExpression =
    (numberJispValue >> Parsed.map Value)
    <|> (stringJispValue >> Parsed.map Value)
    <|> (identifier >> Parsed.map Identifier)
    <|> (Parsers.literal "()" >> Parsed.map (fun _ -> Value (Tuple [])))

exception NotAExpression
let bareExpression = fun input ->
    input |> (
        zeroOrMore (whitespace0 <+@> expression <@+> whitespace0)
        >> Parsed.bind (function
        | [] -> Error NotAExpression
        | x::[] -> Ok x
        | f::args ->
            Apply {
                Function = f
                Arguments = args 
            }
            |> Ok))

let bindExpression = fun input ->
    input |> (
        let openExpr = openBracket <+> Parsers.character '$' >> Parsed.ignore
        openExpr <+@> whitespace0 <+@> 
        identifier <@+> whitespace1 <+> 
        expression <@+> whitespace0 <@+> 
        closeBracket <@+> whitespace0 <+>
        bareExpression)
    |> Parsed.map (fun ((identifier,expr),continuation) ->
        Apply {
            Function = Identifier "$bind"
            Arguments = [
                expr
                Value (Lambda (CustumFunc {
                    Parameters = [identifier]
                    Expression = continuation
                    FunctionContext = Map.empty
                }))
            ]
        })

let ntmBindExpression = fun input ->
    input |> (
        let openExpr = openBracket <+> Parsers.character '~' >> Parsed.ignore
        openExpr <+@> whitespace0 <+@> 
        identifier <@+> whitespace1 <+> 
        zeroOrMore (expression <@+> whitespace0) <@+> 
        closeBracket <@+> whitespace0 <+>
        bareExpression)
    |> Parsed.map (fun ((identifier,expr),continuation) ->
        Apply {
            Function = Identifier "map"
            Arguments = [
                Value (Lambda (CustumFunc {
                    Parameters = [identifier]
                    Expression = continuation
                    FunctionContext = Map.empty
                }))

                Apply {
                    Function = Identifier "tuple"
                    Arguments = expr
                }
            ]
        })

let lambdaExpression = fun input ->
    input |> (
        lambda <+@>
        (oneOrMore (whitespace1 <+@> identifier)) <@+> whitespace1 <+>
        expression)
    |> Parsed.map (fun (parameters,funcExpr) -> 
        {
            Parameters = parameters
            Expression = funcExpr
            FunctionContext = Map.empty
        }
        |> CustumFunc
        |> Lambda
        |> Value)

let expression =
    atomicExpression
    <|> lambdaExpression
    <|> (openBracket <+@> whitespace0 <+@> lambdaExpression <@+> whitespace0 <@+> closeBracket)
    <|> bindExpression
    <|> ntmBindExpression
    <|> (openBracket <+@> whitespace0 <+@> bareExpression <@+> whitespace0 <@+> closeBracket)

