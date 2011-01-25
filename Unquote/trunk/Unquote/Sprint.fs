module Swensen.Unquote.Sprint
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Linq.QuotationEvaluation
open Microsoft.FSharp.Metadata

//precedence: http://msdn.microsoft.com/en-us/library/dd233228.aspx

type binOpAssoc =
    | Left
    | Right
    | Non

let binaryOps = [
    //boolean ops
    "op_Equality", ("=", 13, Left)
    "op_GreaterThan", (">", 13, Left)
    "op_LessThan", ("<", 13, Left)
    "op_GreaterThanOrEqual", (">=", 13, Left)
    "op_LessThanOrEqual", ("<=", 13, Left)
    "op_Inequality", ("<>", 13, Left)
    //pipe ops
    "op_PipeRight", ("|>", 3, Left)
    "op_PipeLeft", ("<|", 3, Left)
    //numeric ops
    "op_Addition", ("+", 17, Left)
    "op_Subtraction", ("-", 17, Left)
    "op_Division", ("/", 18, Left)
    "op_Multiply", ("*", 18, Left)
    "op_Modulus", ("%", 18, Left)
]

//todo: expand to include +, -, *, etc.
let (|BinaryInfixCall|_|) expr =
    match expr with
    | Call (_, mi, lhs::rhs::_) ->
        match binaryOps |> List.tryFind (fst>>((=) mi.Name)) with
        | Some(_,op) -> Some(op,lhs,rhs)
        | None -> None
    | _ -> None

///is the top-level FSI module
let isFsiModule (declaringType:Type) =
    declaringType.Name.StartsWith("FSI_")

//best we can seem to do
let isOpenModule (declaringType:Type) =
    isFsiModule declaringType ||
    declaringType.GetCustomAttributes(true)
    |> Array.tryFind (function | :? AutoOpenAttribute -> true | _ -> false)
    |> (function | Some(_) -> true | None -> false)

///get the source name for the Module or F# Function represented by the given MemberInfo
let sourceName (mi:MemberInfo) =
    mi.GetCustomAttributes(true)
    |> Array.tryPick 
        (function 
            | :? CompilationSourceNameAttribute as csna -> Some(csna.SourceName)
            | :? CompilationRepresentationAttribute as cra -> 
                //seems sufficient, but may not be as robust as FSharpEntity.DisplayName
                if cra.Flags = CompilationRepresentationFlags.ModuleSuffix then 
                    Some(mi.Name.Substring(0, mi.Name.Length - 6))
                else 
                    None
            | _ -> None)
    |> (function | Some(sourceName) -> sourceName | None -> mi.Name)

//todo:
//  remaining binary ops
//  unary ops
//  mutable let bindings
//  new object
//  note: Dictionary<_,_> values are not sprinted as nicely as in FSI, consider using FSI style
let sprint expr =
    let rec sprint context expr =
        let applyParens prec s = if prec > context then s else sprintf "(%s)" s

        match expr with
        | Application (curry, last) -> //application of arguments to a lambda
            applyParens 20 (sprintf "%s %s" (sprint 19 curry) (sprint 20 last))
        | Lambda (var, lambdaOrBody) ->
            let rec loop lambdaOrBody =
                match lambdaOrBody with
                | Lambda(var, lambdaOrBody) -> sprintf "%s %s" var.Name (loop lambdaOrBody)
                | body -> sprintf "-> %s" (sprint 8 body)
            applyParens 6 (sprintf "fun %s %s" (var.Name) (loop lambdaOrBody))
        | BinaryInfixCall((symbol, prec, assoc), lhs, rhs) -> //must come before Call pattern
            let lhsValue, rhsValue = 
                match assoc with
                | Left -> sprint (prec-1) lhs, sprint prec rhs
                | Right -> sprint prec lhs, sprint (prec-1) rhs
                | Non -> sprint prec lhs, sprint prec rhs
            applyParens prec (sprintf "%s %s %s" lhsValue symbol rhsValue)
        | Call(Some(target), mi, args) -> //instance call
            //just assume instance members always have tupled args
            applyParens 24 (sprintf "%s.%s(%s)" (sprint 23 target) mi.Name (sprintTupledArgs args))
        | Call(None, mi, a::b::_) when mi.Name = "op_Range" -> //precedence seems a bit off for op ranges
            sprintf "{%s..%s}" (sprint 23 a) (sprint 23 b)
        | Call(None, mi, a::b::c::_) when mi.Name = "op_RangeStep" ->
            sprintf "{%s..%s..%s}" (sprint 23 a) (sprint 23 b) (sprint 23 c)
        | Call(None, mi, target::args) when mi.DeclaringType.Name = "IntrinsicFunctions" -> //e.g. GetChar, GetArray, GetArray2D
            sprintf "%s.[%s]" (sprint 23 target) (sprintTupledArgs args)
        | Call(None, mi, args) -> //static call
            if FSharpType.IsModule mi.DeclaringType then
                let methodName = sourceName mi
                let sprintedArgs = sprintCurriedArgs args
                if isOpenModule mi.DeclaringType then 
                    applyParens 20 (sprintf "%s %s" methodName sprintedArgs)
                else 
                    applyParens 20 (sprintf "%s.%s %s" (sourceName mi.DeclaringType) methodName sprintedArgs)
            else //assume CompiledName same as SourceName for static members
                applyParens 24 (sprintf "%s.%s(%s)" mi.DeclaringType.Name mi.Name (sprintTupledArgs args))
        | PropertyGet(Some(target), pi, args) -> //instance get
            match pi.Name, args with
            | _, [] -> sprintf "%s.%s" (sprint 23 target) pi.Name
            | "Item", _ -> sprintf "%s.[%s]" (sprint 23 target) (sprintTupledArgs args)
            | _, _ -> sprintf "%s.%s(%s)" (sprint 23 target) pi.Name (sprintTupledArgs args)
        | PropertyGet(None, pi, args) -> //static get (note: can't accept params)
            if isOpenModule pi.DeclaringType then 
                sprintf "%s" pi.Name
            else
                sprintf "%s.%s" pi.DeclaringType.Name pi.Name 
        | Unit -> "()" //must come before Value pattern
        | Value(obj, _) ->
            if obj = null then "null"
            else sprintf "%A" obj
        | NewTuple (args) -> //tuples have at least two elements
            args |> sprintTupledArgs |> sprintf "(%s)"
        | NewArray(_,args) ->
            args |> sprintSequencedArgs |> sprintf "[|%s|]"
        | NewUnionCase(_,_)  ->
            expr.EvalUntyped() |> sprintf "%A"
        | Coerce(target, _) ->
            //don't even "mention" anything about the coersion
            sprint context target
        | Let(var, e1, e2) ->
            //todo: this needs to be handled better for curried functions
            applyParens 5 (sprintf "let %s = %s in %s" var.Name (e1 |> sprint 0) (e2 |> sprint 0))
        | Quote(qx) -> //even though can't reduce due to UntypedEval() limitations
            sprintf "<@ %s @>" (sprint 0 qx)
        | _ -> 
            sprintf "%A" (expr)
    and sprintArgs prec delimiter exprs =
        exprs |> List.map (sprint prec) |> String.concat delimiter
    and sprintTupledArgs = 
        sprintArgs 10 ", "
    and sprintCurriedArgs = //application of arguments to a function
        sprintArgs 20 " "
    and sprintSequencedArgs = //application of arguments to a function
        sprintArgs 4 "; "
    
    sprint 0 expr