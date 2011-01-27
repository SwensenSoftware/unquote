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
//  new object
//  if then else
//  and / or
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
                | body -> sprintf "-> %s" (sprint 6 body)
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
            applyParens 20 (sprintf "%s.%s(%s)" (sprint 22 target) mi.Name (sprintTupledArgs args))
        | Call(None, mi, a::b::_) when mi.Name = "op_Range" -> //not sure about precedence for op ranges
            sprintf "{%s..%s}" (sprint 0 a) (sprint 0 b)
        | Call(None, mi, a::b::c::_) when mi.Name = "op_RangeStep" ->
            sprintf "{%s..%s..%s}" (sprint 0 a) (sprint 0 b) (sprint 0 c)
        | Call(None, mi, target::args) when mi.DeclaringType.Name = "IntrinsicFunctions" -> //e.g. GetChar, GetArray, GetArray2D
            sprintf "%s.[%s]" (sprint 22 target) (sprintTupledArgs args)
        | Call(None, mi, args) -> //static call
            if FSharpType.IsModule mi.DeclaringType then
                let methodName = sourceName mi
                let sprintedArgs = sprintCurriedArgs args
                if isOpenModule mi.DeclaringType then 
                    applyParens 20 (sprintf "%s %s" methodName sprintedArgs)
                else 
                    applyParens 20 (sprintf "%s.%s %s" (sourceName mi.DeclaringType) methodName sprintedArgs)
            else //assume CompiledName same as SourceName for static members
                applyParens 20 (sprintf "%s.%s(%s)" mi.DeclaringType.Name mi.Name (sprintTupledArgs args))
        | PropertyGet(Some(target), pi, args) -> //instance get
            match pi.Name, args with
            | _, [] -> sprintf "%s.%s" (sprint 22 target) pi.Name
            | "Item", _ -> sprintf "%s.[%s]" (sprint 22 target) (sprintTupledArgs args)
            | _, _ -> applyParens 20 (sprintf "%s.%s(%s)" (sprint 22 target) pi.Name (sprintTupledArgs args))
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
        | NewUnionCase(_,_)  -> //needs improvement
            expr.EvalUntyped() |> sprintf "%A"
        | NewObject(ci, args) ->
            applyParens 20 (sprintf "%s(%s)" ci.DeclaringType.Name (sprintTupledArgs args))
        | Coerce(target, _) ->
            //don't even "mention" anything about the coersion
            sprint context target
        | Let(var, e1, e2) ->
            //todo: this needs to be handled better for curried functions
            applyParens 5 (sprintf "let%s%s = %s in %s" (if var.IsMutable then " mutable " else " ") var.Name (e1 |> sprint 0) (e2 |> sprint 0))
        | Quote(qx) -> //even though can't reduce due to UntypedEval() limitations
            sprintf "<@ %s @>" (sprint 0 qx)
        | AndAlso(a,b) -> //must come before if then else
            applyParens 12 (sprintf "%s && %s" (sprint 11 a) (sprint 12 b))
        | OrElse(a,b) -> //must come before if then else
            applyParens 11 (sprintf "%s || %s" (sprint 10 a) (sprint 11 b))
        | IfThenElse(a,b,c) ->
            applyParens 7 (sprintf "if %s then %s else %s" (sprint 7 a) (sprint 7 b) (sprint 7 c))
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

//precedence: http://msdn.microsoft.com/en-us/library/dd233228.aspx
(*
		Operator    	Associativity
	1	as	            Right
	2	when	        Right
	3	| (pipe)	    Left
	4	;	            Right
	5	let	            Nonassociative
	6	function , fun, 
        match, try	    Nonassociative
	7	if	            Nonassociative
	8	->	            Right
	9	:=	            Right
	10	,	            Nonassociative
	11	or , ||	        Left
	12	& , &&	        Left
	13	< op, >op, =, 
        |op, &op	    Left
	14	^ op	        Right
	15	::	            Right
	16	:?> , :?	    Nonassociative
	17	- op, +op, 
        (binary)	    Left
	18	* op, /op, %op	Left
	19	** op	        Right
	20	f x (function 
        application)	Left
	21	| (pattern 
        match)	        Right
	22	prefix ops 
        (+op, -op, %, 
        %%, &, &&, 
        !op, ~op)	    Left
	23	.	            Left
	24	f(x)	        Left
	25	f< types >	    Left
*)