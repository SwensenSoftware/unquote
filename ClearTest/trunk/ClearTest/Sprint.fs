module Swensen.ClearTest.Sprint
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

let binaryOps = [
    //boolean ops
    "op_Equality", "="
    "op_GreaterThan", ">"
    "op_LessThan", "<"
    "op_GreaterThanOrEqual", ">="
    "op_LessThanOrEqual", "<="
    "op_Inequality", "<>"
    //pipe ops
    "op_PipeRight", "|>"
    "op_PipeLeft", "<|"
    //numeric ops
    "op_Addition", "+"
    "op_Subtraction", "-"
    "op_Division", "/"
    "op_Multiply", "*"
    "op_Modulus", "%"
]

//todo: expand to include +, -, *, etc.
let (|BinaryInfixCall|_|) expr =
    match expr with
    | Call (_, mi, args) ->
        match binaryOps |> List.tryFind (fst>>((=) mi.Name)) with
        | Some(_,opStr) -> let lhs::rhs::_ = args in Some(opStr,lhs,rhs)
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
//  add parens based on precedence <-- big one!
//  mutable let bindings
//  new object
//  note: Dictionary<_,_> values are not sprinted as nicely as in FSI, consider using FSI style

//funny case: <@ "asdf".[2] @> resolves as call to
//Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions.GetString
//same with IntrinsicFunctions.GetArray
let rec sprintExpr expr =
    match expr with
    | Application (curry, last) -> //not actually sure what an application is
        sprintf "%s %s" (sprintExpr curry) (sprintExpr last)
    | Lambda (var, lambdaOrBody) ->
        let rec loop lambdaOrBody =
            match lambdaOrBody with
            | Lambda(var, lambdaOrBody) -> sprintf "%s %s" var.Name (loop lambdaOrBody)
            | body -> sprintf "-> %s" (sprintExpr body)
        sprintf "(fun %s %s)" (var.Name) (loop lambdaOrBody) //deal with parens latter
    | BinaryInfixCall(opStr, lhs, rhs) -> //must come before Call pattern
        let lhsValue, rhsValue = sprintExpr lhs, sprintExpr rhs
        sprintf "%s %s %s" lhsValue opStr rhsValue
    | Call(calle, mi, args) ->
        match calle with
        | Some(instanceExpr) -> //instance call
            //just assume instance members always have tupled args
            sprintf "%s.%s(%s)" (sprintExpr instanceExpr) mi.Name (sprintTupledArgs args)
        | None -> //static call
            if FSharpType.IsModule mi.DeclaringType then
                let methodName = sourceName mi
                let sprintedArgs = sprintCurriedArgs args
                if isOpenModule mi.DeclaringType then 
                    sprintf "%s %s" methodName sprintedArgs
                else 
                    sprintf "%s.%s %s" (sourceName mi.DeclaringType) methodName sprintedArgs
            else //assume CompiledName same as SourceName for static members
                sprintf "%s.%s(%s)" mi.DeclaringType.Name mi.Name (sprintTupledArgs args)
    | PropertyGet(calle, pi, args) -> 
        match calle with
        | Some(instanceExpr) -> //instance call 
            match pi.Name, args with
            | _, [] -> sprintf "%s.%s" (sprintExpr instanceExpr) pi.Name
            | "Item", _ -> sprintf "%s.[%s]" (sprintExpr instanceExpr) (sprintTupledArgs args)
            | _, _ -> sprintf "%s.%s(%s)" (sprintExpr instanceExpr) pi.Name (sprintTupledArgs args)
        | None -> //static call (note: can't accept params)
            if isOpenModule pi.DeclaringType then 
                sprintf "%s" pi.Name
            else
                sprintf "%s.%s" pi.DeclaringType.Name pi.Name 
    | Unit -> "()" //must come before Value pattern
    | Value(obj, typeObj) ->
        if obj = null then "null"
        else sprintf "%A" obj
    | NewTuple (args) -> //tuples have at least two elements
        args |> sprintTupledArgs |> sprintf "(%s)"
    | NewUnionCase(_,_) | NewArray(_,_)  ->
        expr.EvalUntyped() |> sprintf "%A"
    | Coerce(target, _) ->
        //don't even "mention" anything about the coersion
        sprintExpr target
    | Let(var, e1, e2) ->
        //todo: this needs to be handled better for curried functions
        sprintf "let %s = %s in %s" var.Name (e1 |> sprintExpr) (e2 |> sprintExpr)
    | Quote(qx) -> 
        sprintf "<@ %s @>" (sprintExpr qx)
    | _ -> 
        sprintf "%A" (expr)

and sprintArgs delimiter exprs =
    exprs |> List.map sprintExpr |> String.concat delimiter
    
and sprintTupledArgs = sprintArgs ", "
and sprintCurriedArgs = sprintArgs " "