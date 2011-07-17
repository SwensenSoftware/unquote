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

open Swensen.Utils
module EP = Swensen.Unquote.ExtraPatterns
module ER = Swensen.Unquote.ExtraReflection
module OP = Swensen.Unquote.OperatorPrecedence
      
//todo:
//  precedence applied to lhs of . not right, see skipped SourceOpTests
//  note: Dictionary<_,_> values are not sprinted as nicely as in FSI, consider using FSI style
let decompile expr =
    let rec decompile precContext expr =
        let applyParens = ER.applyParensForPrecInContext precContext

        match expr with
        | P.Sequential(P.Sequential(lhs, DP.Unit), rhs) ->
            //due to quirky nested structure which handles implicit unit return values
            //need to hack precedence / application of parenthisizes.  we give
            //lhs anecdotally higher precedence context of 10.
            applyParens OP.Semicolon.Prec (sprintf "%s; %s" (decompile 10 lhs) (decompile OP.Semicolon.RightPrec rhs))
        | P.Sequential(lhs, rhs) -> 
            applyParens OP.Semicolon.Prec (sprintf "%s; %s" (decompile OP.Semicolon.LeftPrec lhs) (decompile OP.Semicolon.RightPrec rhs))
        | P.Application(curry, last) -> //application of arguments to a lambda
            applyParens OP.Application.Prec (sprintf "%s %s" (decompile OP.Application.LeftPrec curry) (decompile OP.Application.RightPrec last))
        //issue 25 and issue 23: the following "re-sugars" both partially applied and unapplied lambda call expressions
        //must come before Lambdas
        | EP.IncompleteLambdaCall(mi, args) -> //assume lambdas are only part of modules.
            match EP.binaryOps |> Map.tryFind mi.Name with
                | Some(symbol,_) -> 
                    let sprintedSymbol = sprintf "(%s)" symbol
                    match args.Length with
                    | 1 -> applyParens OP.Application.Prec (sprintf "%s %s" sprintedSymbol (decompileCurriedArgs args))
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
                        else applyParens OP.Application.Prec (sprintf "%s %s" (sprintFunction mi) (decompileCurriedArgs args))
        | DP.Lambdas(vars, body) -> //addresses issue 27
            let sprintSingleVar (var:Var) = if var.Type = typeof<Unit> then "()" else var.Name
            let sprintedVars =
                vars
                |> List.map  
                    (function 
                        | [var] -> sprintSingleVar var 
                        | tupledVars -> sprintf "(%s)" (tupledVars |> List.map sprintSingleVar |> String.concat ", "))
                |> String.concat " "
            applyParens OP.Fun.Prec (sprintf "fun %s -> %s" sprintedVars (decompile 0 body))
        | EP.BinaryInfixCall((symbol, prec), lhs, rhs) -> //must come before Call pattern
            let lhsValue, rhsValue = decompile prec.LeftPrec lhs, decompile prec.RightPrec rhs
            applyParens prec.Prec (sprintf "%s %s %s" lhsValue symbol rhsValue)
        | EP.UnaryPrefixCall(symbol, arg) -> //must come before Call pattern
            applyParens OP.PrefixOps.Prec (sprintf "%s%s" symbol (decompile OP.PrefixOps.Prec arg))
        | P.Call(Some(target), mi, args) -> //instance call
            //just assume instance members always have tupled args
            applyParens OP.Application.Prec (sprintf "%s.%s%s(%s)" (decompile 22 target) mi.Name (ER.sprintGenericArgsIfNotInferable mi) (decompileTupledArgs args))
        | P.Call(None, mi, [lhs]) when mi.Name = "TypeTestGeneric" ->
            //thinking about making decompile depend on Reduce.isReduced: 
            //so that when lhs |> isReduced, print type info for lhs (since would be helpful here)
            //but I think the sprinting of lhs it is reduced conveys type info sufficiently enough
            applyParens OP.TypeTest.Prec (sprintf "%s :? %s" (decompile OP.TypeTest.LeftPrec lhs) (ER.sprintSig (mi.GetGenericArguments().[0])))
        | P.TypeTest(lhs, ty) -> //seems to be same as TypeTestGeneric
            applyParens OP.TypeTest.Prec (sprintf "%s :? %s" (decompile OP.TypeTest.LeftPrec lhs) (ER.sprintSig ty))
        | EP.Range(startToken,endToken,a,b) -> //not sure about precedence for op ranges
            sprintf "%s%s..%s%s" startToken (decompile 0 a) (decompile 0 b) endToken
        | EP.RangeStep(startToken,endToken,a,b,c) ->
            sprintf "%s%s..%s..%s%s" startToken (decompile 0 a) (decompile 0 b) (decompile 0 c) endToken
        | P.Call(None, mi, target::[]) when mi.DeclaringType.Name = "IntrinsicFunctions" && mi.Name = "UnboxGeneric" -> //i.e. :?>
            let ty = mi.GetGenericArguments().[0]
            applyParens OP.DynamicCast.Prec (sprintf "%s :?> %s" (decompile OP.DynamicCast.LeftPrec target) (ER.sprintSig ty))
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
                    applyParens OP.Application.Prec (sprintf "%s%s" methodName sprintedArgs)
                else 
                    applyParens OP.Application.Prec (sprintf "%s.%s%s" (ER.sourceName mi.DeclaringType) methodName sprintedArgs)
            else //assume static calls in non-modules are members for simplicity, also CompiledName same as SourceName
                applyParens OP.Application.Prec (sprintf "%s.%s%s(%s)" mi.DeclaringType.Name mi.Name (ER.sprintGenericArgsIfNotInferable mi) (decompileTupledArgs args))
        | P.PropertyGet(Some(target), pi, args) -> //instance get
            match pi.Name, args with
            | CompiledMatch(@"^Item(\d*)?$") _, _ when pi.DeclaringType |> FSharpType.IsUnion ->
                //for UnionCaseTypeTests, require a op_Dynamic implementation
                sprintf "(%s?%s : %s)" (decompile 22 target) pi.Name (pi.PropertyType |> ER.sprintSig)
            | _, [] -> sprintf "%s.%s" (decompile 22 target) pi.Name //also includes "Item" with zero args
            | "Item", _ -> sprintf "%s.[%s]" (decompile 22 target) (decompileTupledArgs args)
            | _, _ -> applyParens OP.Application.Prec (sprintf "%s.%s(%s)" (decompile 22 target) pi.Name (decompileTupledArgs args))
        | P.PropertyGet(None, pi, args) -> //static get (note: can't accept params)
            let sprintedName =
                if ER.isOpenModule pi.DeclaringType then 
                    sprintf "%s" pi.Name
                else
                    sprintf "%s.%s" pi.DeclaringType.Name pi.Name

            if args.Length = 0 then sprintedName
            else applyParens OP.Application.Prec (sprintf "%s(%s)" sprintedName (decompileTupledArgs args))
        | P.PropertySet(target, pi, piArgs, rhs) ->
            let lhs = //leverage PropertyGet sprinting
                match target with
                | Some(instance) -> Expr.PropertyGet(instance, pi, piArgs)
                | None -> Expr.PropertyGet(pi, piArgs)
            //don't know what precedence is
            applyParens OP.LessThanOp.Prec (sprintf "%s <- %s" (decompile 0 lhs) (decompile 0 rhs))
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
            applyParens OP.LessThanOp.Prec (sprintf "%s <- %s" (decompile 0 lhs) (decompile 0 rhs))
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
                    | lhs::rhs::[] -> applyParens OP.Cons.Prec (sprintf "%s::%s" (decompile OP.Cons.LeftPrec lhs) (decompile OP.Cons.RightPrec rhs))
                    | _ -> failwithf "unexpected list union case: %A" expr
        | P.NewUnionCase(uci,args) -> //"typical union case construction"
            match args with
            | [] -> uci.Name
            | _ -> sprintf "%s(%s)" uci.Name (decompileTupledArgs args)
        | P.NewObject(ci, args) ->
            applyParens OP.Application.Prec (sprintf "new %s(%s)" (ER.sprintSig ci.DeclaringType) (decompileTupledArgs args))
        | P.Coerce(target, _) ->
            //don't even "mention" anything about the coersion
            decompile precContext target
        | EP.TupleLet(vars, e1, e2) ->
            //if any are mutable, they are all mutable
            let anyMutable = vars |> List.exists (function | Some(v) -> v.IsMutable | None -> false)
            let varNames = vars |> List.map (function | Some(v) -> v.Name | None -> "_")
            applyParens OP.Let.Prec (sprintf "let%s%s = %s in %s" (if anyMutable then " mutable " else " ") (varNames |> String.concat ", ") (decompile 0 e1) (decompile 0 e2))
        | P.LetRecursive((firstVar, firstBody)::rest, finalBody) -> //let recursives always have at least thef first var and body
            //note: single line recursive ("and") let bindings are only valid with #light "off", see: http://stackoverflow.com/questions/6501378/what-is-the-non-light-syntax-for-recursive-let-bindings
            let rec decompileRest = function
                | (var:Var, body)::rest ->
                    sprintf " and %s = %s%s" var.Name (decompile 0 body) (decompileRest rest)
                | [] -> sprintf " in %s" (decompile 0 finalBody)
            applyParens OP.Let.Prec (sprintf "let rec %s = %s%s" firstVar.Name (decompile 0 firstBody) (decompileRest rest))
        | P.Let(var, e1, e2) ->
            //todo: this needs to be handled better for curried functions
            applyParens OP.Let.Prec (sprintf "let%s%s = %s in %s" (if var.IsMutable then " mutable " else " ") var.Name (decompile 0 e1) (decompile 0 e2))
        | P.Quote(qx) ->
            //N.B. we have no way of differentiating betweened typed and untyped inner quotations; all come as untyped so that's the only kind we can support.
            sprintf "<@ %s @>" (decompile 0 qx) 
        | DP.OrElse(DP.Bool(true), DP.Bool(false)) -> //true || false can't be distinguished from true && true, yet is less likely an expression due to short-circuiting
            applyParens OP.And.Prec "true && true"
        | DP.AndAlso(DP.Bool(false), DP.Bool(true)) -> //false && true can't be distinguished from false || false, yet is less likely an expression due to short-circuiting
            applyParens OP.Or.Prec "false || false"
        | DP.AndAlso(a,b) -> //must come before if then else
            applyParens OP.And.Prec (sprintf "%s && %s" (decompile 11 a) (decompile 12 b))
        | DP.OrElse(a,b) -> //must come before if then else
            applyParens OP.Or.Prec (sprintf "%s || %s" (decompile 10 a) (decompile 11 b))
        | P.IfThenElse(a,b,c) ->
            applyParens OP.If.Prec (sprintf "if %s then %s else %s" (decompile OP.If.Prec a) (decompile OP.If.Prec b) (decompile OP.If.Prec c))
        //we can't reduce any XXXSet expressions due to limitations of Expr.Eval()
        | P.VarSet(v, arg) ->
            //not sure what precedence should be, using precedence for < op
            applyParens OP.LessThanOp.Prec (sprintf "%s <- %s" v.Name (decompile 0 arg)) 
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
            applyParens OP.If.Prec (sprintf "match %s with | %s -> true | _ -> false" (decompile OP.If.Prec target) ucMatch)
        | P.TryFinally(tryBody, finallyBody) ->
            applyParens OP.Try.Prec (sprintf "try %s finally %s" (decompile OP.Try.Prec tryBody) (decompile OP.Try.Prec finallyBody))
        | P.WhileLoop(condition,body) ->
            applyParens OP.While.Prec (sprintf "while %s do %s" (decompile OP.While.Prec condition) (decompile OP.Try.Prec body))
        | P.ForIntegerRangeLoop(var,rangeStart,rangeEnd,body) ->
            applyParens OP.For.Prec (sprintf "for %s in %s..%s do %s" var.Name (decompile 0 rangeStart) (decompile 0 rangeEnd) (decompile OP.For.Prec body))
        | _ -> 
            sprintf "%A" (expr)
    and decompileArgs prec delimiter exprs =
        exprs |> List.map (decompile prec) |> String.concat delimiter
    and decompileTupledArgs = 
        decompileArgs OP.Comma.Prec ", "
    and decompileCurriedArgs = //application of arguments to a function
        decompileArgs OP.Application.Prec " "
    and decompileSequencedArgs =
        decompileArgs OP.Semicolon.Prec "; "
    decompile 0 expr

//-----operator precedence updated April 2011 with bitwise ops-----
//see OperatorPrecedence.xlsx, not yet implemented.

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