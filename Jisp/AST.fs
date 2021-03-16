module rec Jisp.AST

type JispNumber = System.Decimal

type JispExpr =
| Value of JispValue
| Identifier of string
| Apply of Apply

type JispValue =
| Lambda of JispFunc
| Number of JispNumber
| Tuple of JispValue list

type Apply = {
    Function : JispExpr
    Arguments : JispExpr list }

type CustumFunc = {
    Parameters : string list
    Expression : JispExpr
    FunctionContext : Map<string,JispValue> }

type RuntimeFunc = Context -> JispExpr list -> Result<JispValue,exn>

type JispFunc = 
| CustumFunc of CustumFunc
| RuntimeFunc of RuntimeFunc

type Context = {
    Local : Map<string,JispValue>
    Level : uint64
}
