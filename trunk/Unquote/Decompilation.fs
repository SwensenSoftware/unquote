(*
Copyright 2011 Stephen Swensen

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

module internal Swensen.Unquote.Decompilation
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Metadata

module P = Microsoft.FSharp.Quotations.Patterns
module DP = Microsoft.FSharp.Quotations.DerivedPatterns

open Swensen
module EP = Swensen.Unquote.ExtraPatterns
module ER = Swensen.Unquote.ExtraReflection
                
//todo:
//  precedence applied to lhs of . not right, see skipped SourceOpTests
//  note: Dictionary<_,_> values are not sprinted as nicely as in FSI, consider using FSI style
let decompile expr =
    let rec decompile context expr =
        let applyParens = ER.applyParensForPrecInContext context
        match expr with
        | P.Sequential(P.Sequential(lhs, DP.Unit), rhs) ->
            //due to quirky nested structure which handles implicit unit return values
            //need to hack precedence / application of parenthisizes.  we give
            //lhs anecdotally higher precedence context of 10.
            applyParens 4 (sprintf "%s; %s" (decompile 10 lhs) (decompile 3 rhs))
        | P.Sequential(lhs, rhs) -> 
            applyParens 4 (sprintf "%s; %s" (decompile 4 lhs) (decompile 3 rhs))
        | P.Application(curry, last) -> //application of arguments to a lambda
            applyParens 20 (sprintf "%s %s" (decompile 19 curry) (decompile 20 last))
        //issue 25 and issue 23: the following "re-sugars" both partially applied and unapplied lambda call expressions
        //must come before Lambdas
        | EP.IncompleteLambdaCall(mi, args) -> //assume lambdas are only part of modules.
            match EP.binaryOps |> Map.tryFind mi.Name with
                | Some(symbol,_,_) -> 
                    let sprintedSymbol = sprintf "(%s)" symbol
                    match args.Length with
                    | 1 -> applyParens 20 (sprintf "%s %s" sprintedSymbol (decompileCurriedArgs args))
                    | 0 -> sprintedSymbol
                    | _ -> failwithf "partial applied binary op should only have 0 or 1 args but has more: %A" args
                | None ->
                    match EP.unaryOps |> Map.tryFind mi.Name with
                    | Some(symbol) -> sprintf "(~%s)" symbol
                    | None -> 
                        let sprintFunction (mi:MethodInfo) =
                            if ER.isOpenModule mi.DeclaringType then ER.sourceName mi
                            else sprintf "%s.%s" (ER.sourceName mi.DeclaringType) (ER.sourceName mi)
                        if args.Length = 0 then sprintFunction mi //not sure what precedence should be
                        else applyParens 20 (sprintf "%s %s" (sprintFunction mi) (decompileCurriedArgs args))
        | DP.Lambdas(vars, body) -> //addresses issue 27
            let sprintSingleVar (var:Var) = if var.Type = typeof<Unit> then "()" else var.Name
            let sprintedVars =
                vars
                |> List.map  
                    (function 
                        | [var] -> sprintSingleVar var 
                        | tupledVars -> sprintf "(%s)" (tupledVars |> List.map sprintSingleVar |> String.concat ", "))
                |> String.concat " "
            applyParens 6 (sprintf "fun %s -> %s" sprintedVars (decompile 0 body))
        | EP.BinaryInfixCall((symbol, prec, assoc), lhs, rhs) -> //must come before Call pattern
            let lhsValue, rhsValue = 
                match assoc with
                | EP.Left -> decompile (prec-1) lhs, decompile prec rhs
                | EP.Right -> decompile prec lhs, decompile (prec-1) rhs
                | EP.Non -> decompile prec lhs, decompile prec rhs
            applyParens prec (sprintf "%s %s %s" lhsValue symbol rhsValue)
        | EP.UnaryPrefixCall(symbol, arg) -> //must come before Call pattern
            applyParens 22 (sprintf "%s%s" symbol (decompile 22 arg))
        | P.Call(Some(target), mi, args) -> //instance call
            //just assume instance members always have tupled args
            applyParens 20 (sprintf "%s.%s%s(%s)" (decompile 22 target) mi.Name (ER.sprintGenericArgsIfNotInferable mi) (decompileTupledArgs args))
        | P.Call(None, mi, [lhs]) when mi.Name = "TypeTestGeneric" ->
            //thinking about making decompile depend on Reduce.isReduced: 
            //so that when lhs |> isReduced, print type info for lhs (since would be helpful here)
            //but I think the sprinting of lhs it is reduced conveys type info sufficiently enough
            applyParens 16 (sprintf "%s :? %s" (decompile 16 lhs) (ER.sprintSig (mi.GetGenericArguments().[0])))
        | EP.Range(startToken,endToken,a,b) -> //not sure about precedence for op ranges
            sprintf "%s%s..%s%s" startToken (decompile 0 a) (decompile 0 b) endToken
        | EP.RangeStep(startToken,endToken,a,b,c) ->
            sprintf "%s%s..%s..%s%s" startToken (decompile 0 a) (decompile 0 b) (decompile 0 c) endToken
        | P.Call(None, mi, target::[]) when mi.DeclaringType.Name = "IntrinsicFunctions" && mi.Name = "UnboxGeneric" -> //i.e. :?>
            let ty = mi.GetGenericArguments().[0]
            applyParens 16 (sprintf "%s :?> %s" (decompile 16 target) (ER.sprintSig ty))
        | P.Call(None, mi, target::args) when mi.DeclaringType.Name = "IntrinsicFunctions" -> //e.g. GetChar, GetArray, GetArray2D
            sprintf "%s.[%s]" (decompile 22 target) (decompileTupledArgs args) //not sure what precedence is
        | P.Call(None, mi, args) -> //static call (we assume F# functions are always static calls for simplicity)
            if FSharpType.IsModule mi.DeclaringType then //we assume static calls in modules are functions, for simplicity
                let methodName = ER.sourceName mi
                                
                //if mi has generic args which can't be infered, need to decompile them.
                //if mi takes no arguments, then need to decompile "()", unless mi is an F# value, in which case we omit ()
                let sprintedArgs = 
                    sprintf "%s%s"
                        (if ER.genericArgsInferable mi then "" else ER.sprintGenericArgs mi) 
                        (if args.Length = 0 then 
                            if ER.isGenericValue(mi) then ""
                            else "()"
                         else " " + decompileCurriedArgs args)
                
                if ER.isOpenModule mi.DeclaringType then 
                    applyParens 20 (sprintf "%s%s" methodName sprintedArgs)
                else 
                    applyParens 20 (sprintf "%s.%s%s" (ER.sourceName mi.DeclaringType) methodName sprintedArgs)
            else //assume static calls in non-modules are members for simplicity, also CompiledName same as SourceName
                applyParens 20 (sprintf "%s.%s%s(%s)" mi.DeclaringType.Name mi.Name (ER.sprintGenericArgsIfNotInferable mi) (decompileTupledArgs args))
        | P.PropertyGet(Some(target), pi, args) -> //instance get
            match pi.Name, args with
            | CompiledMatch(@"^Item(\d*)?$") _, _ when pi.DeclaringType |> FSharpType.IsUnion ->
                //for UnionCaseTypeTests, require a op_Dynamic implementation
                sprintf "(%s?%s : %s)" (decompile 22 target) pi.Name (pi.PropertyType |> ER.sprintSig)
            | _, [] -> sprintf "%s.%s" (decompile 22 target) pi.Name //also includes "Item" with zero args
            | "Item", _ -> sprintf "%s.[%s]" (decompile 22 target) (decompileTupledArgs args)
            | _, _ -> applyParens 20 (sprintf "%s.%s(%s)" (decompile 22 target) pi.Name (decompileTupledArgs args))
        | P.PropertyGet(None, pi, args) -> //static get (note: can't accept params)
            let sprintedName =
                if ER.isOpenModule pi.DeclaringType then 
                    sprintf "%s" pi.Name
                else
                    sprintf "%s.%s" pi.DeclaringType.Name pi.Name

            if args.Length = 0 then sprintedName
            else applyParens 20 (sprintf "%s(%s)" sprintedName (decompileTupledArgs args))
        | P.PropertySet(target, pi, piArgs, rhs) ->
            let lhs = //leverage PropertyGet sprinting
                match target with
                | Some(instance) -> Expr.PropertyGet(instance, pi, piArgs)
                | None -> Expr.PropertyGet(pi, piArgs)
            //don't know what precedence is
            applyParens 13 (sprintf "%s <- %s" (decompile 0 lhs) (decompile 0 rhs))
        | P.FieldGet(Some(target), fi) ->
            sprintf "%s.%s" (decompile 22 target) fi.Name
        | P.FieldGet(None, fi) ->
            sprintf "%s.%s" fi.DeclaringType.Name fi.Name
        | P.FieldSet(target, fi, rhs) ->
            let lhs = //leverage FieldGet sprinting
                match target with
                | Some(instance) -> Expr.FieldGet(instance, fi) 
                | None -> Expr.FieldGet(fi)
            //don't know what precedence is
            applyParens 13 (sprintf "%s <- %s" (decompile 0 lhs) (decompile 0 rhs))
        | DP.Unit -> "()" //must come before Value pattern
        | P.Value(o, _) ->
            match o with
            | null -> "null"
            | _ -> sprintf "%A" o
        | P.NewTuple(args) -> //tuples have at least two elements
            args |> decompileTupledArgs |> sprintf "(%s)" //what is precedence? 10?
        | P.NewArray(_,args) ->
            args |> decompileSequencedArgs |> sprintf "[|%s|]"
        //list union cases more complex than normal union cases since need to consider
        //both cons infix operator and literal list constructions.
        | P.NewUnionCase(uci,args) when uci |> ER.isListUnionCase ->
            if args = [] then
                "[]"
            else
                let rec isLiteralConstruction = function
                    | P.NewUnionCase(_, lhs::(P.NewUnionCase(_, []))::[]) -> true //e.g. _::_::...::[]
                    | P.NewUnionCase(_, lhs::rhs::[]) ->
                        match rhs with
                        | P.NewUnionCase _ -> isLiteralConstruction rhs //e.g. _::_::...
                        | _ -> false //e.g. _::_::x
                    | _ -> failwith "unexpected list union case"

                if expr |> isLiteralConstruction then
                    let rec sprintLiteralConstructionArgs = function
                        | P.NewUnionCase(_, lhs::(P.NewUnionCase(_, []))::[]) -> decompile 4 lhs
                        | P.NewUnionCase(_, lhs::rhs::[]) ->
                            sprintf "%s; %s" (decompile 4 lhs) (sprintLiteralConstructionArgs rhs)
                        | _ -> failwith "unexpected list union case"
                    sprintf "[%s]" (sprintLiteralConstructionArgs expr)
                else 
                    //would like to optimize somehow so isLiteralConstruction is not called with every recursive 
                    //decompile of non literal constructions.
                    match args with
                    | lhs::rhs::[] -> applyParens 15 (sprintf "%s::%s" (decompile 15 lhs) (decompile 14 rhs))
                    | _ -> failwithf "unexpected list union case: %A" expr
        | P.NewUnionCase(uci,args) -> //"typical union case construction"
            match args with
            | [] -> uci.Name
            | _ -> sprintf "%s(%s)" uci.Name (decompileTupledArgs args)
        | P.NewObject(ci, args) ->
            applyParens 20 (sprintf "new %s(%s)" (ER.sprintSig ci.DeclaringType) (decompileTupledArgs args))
        | P.Coerce(target, _) ->
            //don't even "mention" anything about the coersion
            decompile context target
        | EP.TupleLet(vars, e1, e2) ->
            //if any are mutable, they are all mutable
            let anyMutable = vars |> List.exists (function | Some(v) -> v.IsMutable | None -> false)
            let varNames = vars |> List.map (function | Some(v) -> v.Name | None -> "_")
            applyParens 5 (sprintf "let%s%s = %s in %s" (if anyMutable then " mutable " else " ") (varNames |> String.concat ", ") (decompile 0 e1) (decompile 0 e2))
        | P.Let(var, e1, e2) ->
            //todo: this needs to be handled better for curried functions
            applyParens 5 (sprintf "let%s%s = %s in %s" (if var.IsMutable then " mutable " else " ") var.Name (decompile 0 e1) (decompile 0 e2))
        | P.Quote(qx) -> //even though can't reduce due to UntypedEval() limitations
            //note, this only handles typed quotations
            sprintf "<@ %s @>" (decompile 0 qx) 
        | DP.AndAlso(a,b) -> //must come before if then else
            applyParens 12 (sprintf "%s && %s" (decompile 11 a) (decompile 12 b))
        | DP.OrElse(a,b) -> //must come before if then else
            applyParens 11 (sprintf "%s || %s" (decompile 10 a) (decompile 11 b))
        | P.IfThenElse(a,b,c) ->
            applyParens 7 (sprintf "if %s then %s else %s" (decompile 7 a) (decompile 7 b) (decompile 7 c))
        //we can't reduce any XXXSet expressions due to limitations of Expr.Eval()
        | P.VarSet(v, arg) ->
            //not sure what precedence should be, using precedence for < op
            applyParens 13 (sprintf "%s <- %s" v.Name (decompile 0 arg)) 
        //extremely verbose
        | P.UnionCaseTest(target, uci) ->
            let ucMatch =
                if uci |> ER.isListUnionCase then
                    if uci.Name = "Empty" then "[]"
                    else "_::_" //"Cons"
                else
                    let len = uci.GetFields().Length
                    if len = 0 then
                        sprintf "%s" uci.Name
                    else
                        sprintf "%s(%s)" uci.Name ("_" |> Array.create len |> String.concat ",")

            //using same precedence as if, 7, for match xxx with
            applyParens 7 (sprintf "match %s with | %s -> true | _ -> false" (decompile 7 target) ucMatch)
        | _ -> 
            sprintf "%A" (expr)
    and decompileArgs prec delimiter exprs =
        exprs |> List.map (decompile prec) |> String.concat delimiter
    and decompileTupledArgs = 
        decompileArgs 10 ", "
    and decompileCurriedArgs = //application of arguments to a function
        decompileArgs 20 " "
    and decompileSequencedArgs =
        decompileArgs 4 "; "
    
    decompile 0 expr

//-----precedence-----
//note: http://stackoverflow.com/questions/4859151/help-me-understand-lambda-expression-precedence
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