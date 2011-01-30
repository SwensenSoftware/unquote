module internal Swensen.Unquote.Sprint
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
    "op_PipeRight2", ("||>", 3, Left)
    "op_PipeRight3", ("|||>", 3, Left)
    "op_PipeLeft", ("<|", 3, Left)
    "op_PipeLeft2", ("<||", 3, Left)
    "op_PipeLeft3", ("<|||", 3, Left)
    //numeric ops
    "op_Addition", ("+", 17, Left)
    "op_Subtraction", ("-", 17, Left)
    "op_Division", ("/", 18, Left)
    "op_Multiply", ("*", 18, Left)
    "op_Modulus", ("%", 18, Left)
    "op_Exponentiation", ("**", 19, Left)
    //bit operators
    "op_BitwiseAnd", ("&&&", 13, Left)
    "op_BitwiseOr", ("|||", 13, Left)
    "op_ExclusiveOr", ("^^^", 14, Right)
    "op_LeftShift", ("<<<", 13, Left)
    "op_RightShift", (">>>", 13, Left)

    //composition
    "op_ComposeRight", (">>", 13, Left)
    "op_ComposeLeft", ("<<", 13, Left)
    //special
    "op_Append", ("@", 17, Left) //not sure what precedence, falling back on (+)
    "op_Concatenate", ("^", 14, Right) //ocaml style string concatentation
]

//future feature, support custom ops
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

//fssnip
module RegexUtils =
    open System.Text.RegularExpressions
    //Regex.CacheSize <- (default is 15)
    ///Match the pattern using a cached interpreted Regex
    let (|InterpretedMatch|_|) (pattern) str =
       let m = Regex.Match(str, pattern) //we can expect 
       if m.Success then Some ([for x in m.Groups -> x])
       else None
    
    ///Match the pattern using a cached compiled Regex
    let (|CompiledMatch|_|) (pattern) str =
       let m = Regex.Match(str, pattern, RegexOptions.Compiled) //we can expect 
       if m.Success then Some ([for x in m.Groups -> x])
       else None

open RegexUtils

//used by both sprintSig and sprint
let applyParens context prec s = if prec > context then s else sprintf "(%s)" s

//the usefullness of this function makes me think to open up Sprint module
///Sprint the F#-style type signature of the given Type
let sprintSig =
    //list of F# type abbrs: http://207.46.16.248/en-us/library/ee353649.aspx
    ///Get the type abbr name or short name from the "clean" name
    let displayName = function
        | "System.Object"   -> "obj"
        | "System.String"   -> "string"
        | "System.Char"     -> "char"
        | "System.Boolean"  -> "bool"
        | "System.Decimal"  -> "decimal"
        
        | "System.Int16"    -> "int16"
        | "System.Int32"    -> "int"//int32
        | "System.Int64"    -> "int64"
        
        | "System.UInt16"   -> "uint16"
        | "System.UInt32"   -> "uint32"
        | "System.UInt64"   -> "uint64"
        
        | "System.Single"   -> "float32"//single
        | "System.Double"   -> "float"//double
        
        | "System.Byte"     -> "byte"//uint8
        | "System.SByte"    -> "sbyte"//int8

        | "System.IntPtr"   -> "nativeint"
        | "System.UIntPtr"  -> "unativeint"

        | "System.Numerics.BigInteger"  -> "bigint"
        | "Microsoft.FSharp.Core.Unit"  -> "unit"
        | "Microsoft.FSharp.Math.BigRational"   -> "BigNum"
        | "Microsoft.FSharp.Core.FSharpRef"     -> "ref"
        | "Microsoft.FSharp.Core.FSharpOption"  -> "option"
        | "Microsoft.FSharp.Collections.FSharpList" -> "list"
        | "Microsoft.FSharp.Collections.FSharpMap"  -> "Map"
        | "System.Collections.Generic.IEnumerable"  -> "seq"
        | CompiledMatch @"\.?([^\.]*)$" [_;nameMatch] -> nameMatch.Value //short name
        | cleanName -> failwith "failed to lookup type display name from it's \"clean\" name: " + cleanName

    let rec sprintSig context (t:Type) =
        let applyParens = applyParens context
        let cleanName, arrSig = 
            match t.FullName with
            | CompiledMatch @"^([^`\[]*)`?.*?(\[[\[\],]*\])?$" [_;cleanNameMatch;arrSigMatch] -> 
                cleanNameMatch.Value, arrSigMatch.Value
            | _ -> 
                failwith "failed to parse type name: " t.FullName

        let args = t.GetGenericArguments()
        match args.Length with
        | 0 -> (displayName cleanName) + arrSig
        | _ when cleanName = "System.Tuple" ->
            (applyParens (if arrSig.Length > 0 then 0 else 2) (sprintf "%s" (args |> Array.map (sprintSig 2) |> String.concat " * "))) +  arrSig
        | _ ->
            sprintf "%s<%s>%s" (displayName cleanName) (args |> Array.map (sprintSig 1) |> String.concat ", ") arrSig
    
    fun t -> sprintSig 0 t

//todo:
//  unary ops
//  note: Dictionary<_,_> values are not sprinted as nicely as in FSI, consider using FSI style
//  need to look into DerivedPatterns.Lambdas and DerivedPatterns.Applications
//  implement TypeTestGeneric call handling (:?)
//  maybe should expand to VarSet and Sequence
let sprint expr =
    let rec sprint context expr =
        let applyParens = applyParens context

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
        | Call(None, mi, [lhs]) when mi.Name = "TypeTestGeneric" ->
            //thinking about making sprint depend on Reduce.isReduced: 
            //so that when lhs |> isReduced, print type info for lhs (since would be helpful here)
            //but I think the sprinting of lhs it is reduced conveys type info sufficiently enough
            applyParens 16 (sprintf "%s :? %s" (sprint 16 lhs) (sprintSig (mi.GetGenericArguments().[0])))
        | Call(None, mi, a::b::_) when mi.Name = "op_Range" -> //not sure about precedence for op ranges
            sprintf "{%s..%s}" (sprint 0 a) (sprint 0 b)
        | Call(None, mi, a::b::c::_) when mi.Name = "op_RangeStep" ->
            sprintf "{%s..%s..%s}" (sprint 0 a) (sprint 0 b) (sprint 0 c)
        | Call(None, mi, target::args) when mi.DeclaringType.Name = "IntrinsicFunctions" -> //e.g. GetChar, GetArray, GetArray2D
            sprintf "%s.[%s]" (sprint 22 target) (sprintTupledArgs args) //not sure what precedence is
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
            //note, this only handles typed quotations (ops are 
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
//-----precedence-----
//spec: http://research.microsoft.com/en-us/um/cambridge/projects/fsharp/manual/spec.html
//from spec:  Paren(token) pushed when (, begin, struct, sig, {, [, [|, or quote-op-left is encountered.
//custom operator precedence determined by first op in sequence: http://stackoverflow.com/questions/3347972/f-custom-operators-precedence
//precedence table: http://msdn.microsoft.com/en-us/library/dd233228.aspx
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


//operator lookup (from spec)
(*

[]    op_Nil

::    op_ColonColon

+     op_Addition

-     op_Subtraction

*     op_Multiply

/     op_Division

**    op_Exponentiation

@     op_Append       

^     op_Concatenate  

%     op_Modulus

&&&   op_BitwiseAnd

|||   op_BitwiseOr

^^^   op_ExclusiveOr

<<<   op_LeftShift

~~~   op_LogicalNot

>>>   op_RightShift

~+    op_UnaryPlus

~-    op_UnaryNegation

=     op_Equality

<>    op_Inequality

<=    op_LessThanOrEqual

>=    op_GreaterThanOrEqual

<     op_LessThan

>     op_GreaterThan

?     op_Dynamic

?<-   op_DynamicAssignment

|>    op_PipeRight

||>   op_PipeRight2

|||>  op_PipeRight3

<|    op_PipeLeft

<||   op_PipeLeft2

<|||  op_PipeLeft3

!     op_Dereference

>>    op_ComposeRight

<<    op_ComposeLeft

<@ @> op_Quotation

<@@ @@> op_QuotationUntyped

~%    op_Splice

~%%   op_SpliceUntyped

~&    op_AddressOf

~&&   op_IntegerAddressOf

||    op_BooleanOr

&&    op_BooleanAnd

+=    op_AdditionAssignment

-=    op_SubtractionAssignment

*=    op_MultiplyAssignment

/=    op_DivisionAssignment

..    op_Range

.. .. op_RangeStep

*)


