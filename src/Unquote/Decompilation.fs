module internal Swensen.Unquote.Decompilation
open System
open System.Reflection
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations

module P = Microsoft.FSharp.Quotations.Patterns
module DP = Microsoft.FSharp.Quotations.DerivedPatterns

open Swensen.Utils
module EP = Swensen.Unquote.ExtraPatterns
module ER = Swensen.Unquote.ExtraReflection
module OP = Swensen.Unquote.OperatorPrecedence
type OP = OP.OperatorPrecedence

module CustomContext =
    let Zero = (OP(0),OP.Non)

module CC = CustomContext

//todo:
//  precedence applied to lhs of . not right, see skipped SourceOpTests
//  note: Dictionary<_,_> values are not sprinted as nicely as in FSI, consider using FSI style
let decompile expr =
    let rec decompile (contextOP,contextAssoc) expr =
        let applyParens = OP.applyParensForPrecInContext contextOP contextAssoc

        ///Decompile property access for PropertyGet and PropertySet
        let decomplePropertyAccess target (pi:PropertyInfo) args =
            match target with
            | Some(target) -> //instance get
                match pi.Name, args with
                | Regex.Compiled.Match(@"^Item(\d*)?$") _, _ when pi.DeclaringType |> FSharpType.IsUnion ->
                    //for UnionCaseTypeTests, require a op_Dynamic implementation
                    sprintf "(%s?%s : %s)" (decompile (OP.Dot,OP.Left) target) pi.Name (pi.PropertyType |> ER.sprintSig)
                | _, [] -> sprintf "%s.%s" (decompile (OP.Dot,OP.Left) target) pi.Name //also includes "Item" with zero args
                | "Item", _ -> sprintf "%s.[%s]" (decompile (OP.Dot,OP.Left) target) (decompileTupledArgs args)
                | _, _ -> applyParens OP.MethodCall (sprintf "%s.%s(%s)" (decompile (OP.Dot,OP.Left) target) pi.Name (decompileTupledArgs args))
            | None -> //static get (note: can't accept params)
                let sprintedName =
                    if ER.isOpenModule pi.DeclaringType then
                        sprintf "%s" pi.Name
                    else
                        sprintf "%s.%s" pi.DeclaringType.Name pi.Name

                if args.Length = 0 then sprintedName
                else applyParens OP.MethodCall (sprintf "%s(%s)" sprintedName (decompileTupledArgs args))

        ///Decompile field access for PropertyGet and PropertySet
        let decompileFieldAccess target (fi:FieldInfo) =
            match target with
            | Some(target) ->
                applyParens OP.Dot (sprintf "%s.%s" (decompile (OP.Dot,OP.Left) target) fi.Name)
            | None ->
                applyParens OP.Dot (sprintf "%s.%s" fi.DeclaringType.Name fi.Name)

        match expr with
        | P.Sequential(P.Sequential(lhs, DP.Unit), rhs) ->
            //due to quirky nested structure which handles implicit unit return values
            //need to hack precedence / application of parenthisizes.  we give
            //lhs anecdotally higher precedence context of 10.
            applyParens OP.Semicolon (sprintf "%s; %s" (decompile (OP(10), OP.Non) lhs) (decompile (OP.Semicolon, OP.Right) rhs))
        | P.Sequential(lhs, rhs) ->
            applyParens OP.Semicolon (sprintf "%s; %s" (decompile (OP.Semicolon, OP.Left) lhs) (decompile (OP.Semicolon, OP.Right) rhs))
        | EP.Range(startToken,endToken,a,b) -> //not sure about precedence for op ranges. must come before xxxCallOrApplication
            sprintf "%s%s..%s%s" startToken (decompile CC.Zero a) (decompile CC.Zero b) endToken
        | EP.RangeStep(startToken,endToken,a,b,c) -> //must come before xxxCallOrApplication
            sprintf "%s%s..%s..%s%s" startToken (decompile CC.Zero a) (decompile CC.Zero b) (decompile CC.Zero c) endToken
        | EP.InfixCallOrApplication((symbol, prec), lhs, rhs) -> //must come before Call and Application patterns
            let lhsValue, rhsValue = decompile (prec,OP.Left) lhs, decompile (prec,OP.Right) rhs
            applyParens prec (sprintf "%s %s %s" lhsValue symbol rhsValue)
        | EP.PrefixCallOrApplication(symbol, arg) -> //must come before Call and Application patterns
            applyParens OP.PrefixOps (sprintf "%s%s" symbol (decompile (OP.PrefixOps,OP.Non) arg))
        | P.Application(curry, last) -> //application of arguments to a lambda
            applyParens OP.Application (sprintf "%s %s" (decompile (OP.Application, OP.Left) curry) (decompile (OP.Application, OP.Right) last))
        //issue 25 and issue 23: the following "re-sugars" both partially applied and unapplied lambda call expressions
        //must come before Lambdas
        | EP.IncompleteLambdaCall(target, mi, suppliedArgs) -> //assume lambdas are only part of modules.
            //function name: includes support of first-class infix and prefix operators.
            let funName =
                if ER.isOpenModule mi.DeclaringType then ER.sourceName mi
                else
                    let decompiledTarget =
                        match target with
                        | Some(target) -> (decompile (OP.Dot,OP.Left) target) //instance
                        | None -> ER.sourceName <| mi.DeclaringType.GetTypeInfo()
                    sprintf "%s.%s" decompiledTarget (ER.sourceName mi)

            match suppliedArgs.Length with
            | 0 -> funName
            | _ -> applyParens OP.Application (sprintf "%s %s" funName (decompileCurriedArgs suppliedArgs))
        | DP.Lambdas(vars, body) -> //addresses issue 27
            let sprintSingleVar (var:Var) = if var.Type = typeof<Unit> then "()" else var.Name
            let sprintedVars =
                vars
                |> List.map
                    (function
                        | [var] -> sprintSingleVar var
                        | tupledVars -> sprintf "(%s)" (tupledVars |> List.map sprintSingleVar |> String.concat ", "))
                |> String.concat " "
            applyParens OP.Fun (sprintf "fun %s -> %s" sprintedVars (decompile CC.Zero body))
        | P.Call(None, mi, [lhs]) when mi.Name = "TypeTestGeneric" ->
            //thinking about making decompile depend on Reduce.isReduced:
            //so that when lhs |> isReduced, print type info for lhs (since would be helpful here)
            //but I think the sprinting of lhs it is reduced conveys type info sufficiently enough
            applyParens OP.TypeTest (sprintf "%s :? %s" (decompile (OP.TypeTest,OP.Left) lhs) (ER.sprintSig (mi.GetGenericArguments().[0])))
        | P.TypeTest(lhs, ty) -> //seems to be same as TypeTestGeneric
            applyParens OP.TypeTest (sprintf "%s :? %s" (decompile (OP.TypeTest,OP.Left) lhs) (ER.sprintSig ty))
        | EP.NumericLiteral(literalValue, suffix) ->
            literalValue + suffix
        | P.Call(None, mi, target::[]) when mi.DeclaringType.Name = "IntrinsicFunctions" && mi.Name = "UnboxGeneric" -> //i.e. :?>
            let ty = mi.GetGenericArguments().[0]
            applyParens OP.DynamicCast (sprintf "%s :?> %s" (decompile (OP.DynamicCast,OP.Left) target) (ER.sprintSig ty))
        | P.Call(None, mi, target::args) when mi.DeclaringType.Name = "IntrinsicFunctions" -> //e.g. GetChar, GetArray, GetArray2D
            applyParens OP.Dot (sprintf "%s.[%s]" (decompile (OP.Dot, OP.Left) target) (decompileTupledArgs args)) //not sure what precedence is
        | P.Call(None, mi, [P.Lambda(_, arg)]) when mi.Name = "Create" && mi.DeclaringType.FullName = "Microsoft.FSharp.Control.LazyExtensions" -> //System.Lazy.Create treated as function application
            applyParens OP.Application (sprintf "lazy %s" (decompile (OP.Application, OP.Right) arg))
        | P.Call(None, mi, [arg]) when mi.Name = "Assert" && mi.DeclaringType = typeof<System.Diagnostics.Debug> -> //System.Lazy.Create treated as function application
            applyParens OP.Application (sprintf "assert %s" (decompile (OP.Application, OP.Right) arg))
        | P.Call(target, (ER.FunctionOrGenericValue(fOrGV) as mi), args) -> //instance or static call representing an F# function or generic value
            //if mi has generic args which can't be infered, need to sprint them.
            //if mi takes no arguments, then need to decompile "()", unless mi is an F# value, in which case we omit ()
            let sprintedArgs =
                sprintf "%s%s"
                    (if ER.genericArgsInferable mi then "" else ER.sprintGenericArgs mi)
                    (if args.Length = 0 then
                        match fOrGV with
                        | ER.GenericValue -> ""
                        | ER.Function -> "()"
                     else " " + decompileCurriedArgs args)

            let methodName = ER.sourceName mi
            if ER.isOpenModule mi.DeclaringType then
                applyParens OP.Application (sprintf "%s%s" methodName sprintedArgs)
            else
                let decompiledTarget =
                    match target with
                    | Some(target) -> (decompile (OP.Dot,OP.Left) target) //instance
                    | None -> ER.sourceName <| mi.DeclaringType.GetTypeInfo()

                applyParens OP.Application (sprintf "%s.%s%s" decompiledTarget methodName sprintedArgs)
        | P.Call(target, mi, args) -> //a "normal" .net instance or static call
            let decompiledTarget =
                match target with
                | Some(target) -> (decompile (OP.Dot,OP.Left) target) //instance
                | None -> mi.DeclaringType.Name
            applyParens OP.MethodCall (sprintf "%s.%s%s(%s)" decompiledTarget mi.Name (ER.sprintGenericArgsIfNotInferable mi) (decompileTupledArgs args))
        | P.PropertyGet(target, pi, args) ->
            decomplePropertyAccess target pi args
        | P.PropertySet(target, pi, args, rhs) ->
            //don't know what precedence is
            applyParens OP.LessThanOp (sprintf "%s <- %s" (decomplePropertyAccess target pi args) (decompile CC.Zero rhs))
        | P.FieldGet(target, fi) ->
            decompileFieldAccess target fi
        | P.FieldSet(target, fi, rhs) ->
            //don't know what precedence is
            applyParens OP.LessThanOp (sprintf "%s <- %s" (decompileFieldAccess target fi) (decompile CC.Zero rhs))
        | DP.Unit -> "()" //must come before Value pattern
        | EP.LambdaValue(name) -> ER.sourceNameFromString name
        | P.ValueWithName(_, _, name) ->
            ER.sourceNameFromString name
        | P.Value(o, ty) ->
            match o with
            | null when ty |> ER.isGenericTypeDefinedFrom<option<_>> -> "None" //option<_> None is represented as null
            | null -> "null"
            | :? String as x ->
                let pairs = [
                    ("\\", @"\\")
                    ("\"", "\\\"")
                    ("\b", @"\b")
                    ("\t", @"\t")
                    ("\r", @"\r")
                    ("\n", @"\n")]
                let sb = System.Text.StringBuilder(x.Length + 2 + Math.Min(x.Length, 16))
                sb.Append('"').Append(x).Append('"') |> ignore
                pairs |> List.iter(fun (key, value) -> sb.Replace(key, value, 1, sb.Length-2) |> ignore)
                sb.ToString()
            | :? ReductionException as x ->
                sprintf "%A" x.InnerException
            | :? Exception as x ->
                sprintf "%s: %s" (x.GetType().FullName) x.Message
            | _ -> sprintf "%A" o
        | P.NewTuple(args) -> //tuples have at least two elements
            args |> decompileTupledArgs |> sprintf "(%s)" //what is precedence? 10?
        | P.NewArray(_,args) ->
            args |> decompileSequencedArgs |> sprintf "[|%s|]"
        | P.NewRecord(ty,args) ->
            let isAnon =
                ty.FullName.Contains("AnonymousType") &&
                ty.GetCustomAttributes(typeof<System.Runtime.CompilerServices.CompilerGeneratedAttribute>, false).Length > 0

            let body =
                let fields = FSharpType.GetRecordFields(ty)
                (fields,args)
                ||> Seq.map2 (fun f x -> sprintf "%s = %s" f.Name (decompile (OP.Semicolon, OP.Non) x))
                |> String.concat "; "

            let maybePipe = if isAnon then "|" else ""
            sprintf "{%s %s %s}" maybePipe body maybePipe

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
                        | P.NewUnionCase(_, lhs::(P.NewUnionCase(_, []))::[]) -> decompile (OP.Semicolon,OP.Non) lhs
                        | P.NewUnionCase(_, lhs::rhs::[]) ->
                            sprintf "%s; %s" (decompile (OP.Semicolon,OP.Non) lhs) (sprintLiteralConstructionArgs rhs)
                        | _ -> failwith "unexpected list union case"
                    sprintf "[%s]" (sprintLiteralConstructionArgs expr)
                else
                    //would like to optimize somehow so isLiteralConstruction is not called with every recursive
                    //decompile of non literal constructions.
                    match args with
                    | lhs::rhs::[] -> applyParens OP.Cons (sprintf "%s::%s" (decompile (OP.Cons,OP.Left) lhs) (decompile (OP.Cons,OP.Right) rhs))
                    | _ -> failwithf "unexpected list union case: %A" expr
        | P.NewUnionCase(uci,args) -> //"typical union case construction"
            match args with
            | [] -> uci.Name
            | _ -> sprintf "%s(%s)" uci.Name (decompileTupledArgs args)
        | P.NewObject(ci, args) ->
            applyParens OP.Application (sprintf "new %s(%s)" (ER.sprintSig ci.DeclaringType) (decompileTupledArgs args))
        | P.DefaultValue(ty) ->
            applyParens OP.Application (sprintf "new %s()" (ER.sprintSig ty))
        | P.Coerce(target, _) | P.WithValue(_, _, target) ->
            //don't even "mention" anything about the coersion (pass through context)
            decompile (contextOP,contextAssoc) target
        | EP.TupleLet(vars, e1, e2) ->
            //if any are mutable, they are all mutable
            let anyMutable = vars |> List.exists (function | Some(v) -> v.IsMutable | None -> false)
            let varNames = vars |> List.map (function | Some(v) -> v.Name | None -> "_")
            applyParens OP.Let (sprintf "let%s%s = %s in %s" (if anyMutable then " mutable " else " ") (varNames |> String.concat ", ") (decompile CC.Zero e1) (decompile CC.Zero e2))
        | P.LetRecursive((firstVar, firstBody)::rest, finalBody) -> //let recursives always have at least thef first var and body
            //note: single line recursive ("and") let bindings are only valid with #light "off", see: http://stackoverflow.com/questions/6501378/what-is-the-non-light-syntax-for-recursive-let-bindings
            let rec decompileRest = function
                | (var:Var, body)::rest ->
                    sprintf " and %s = %s%s" var.Name (decompile CC.Zero body) (decompileRest rest)
                | [] -> sprintf " in %s" (decompile CC.Zero finalBody)
            applyParens OP.Let (sprintf "let rec %s = %s%s" firstVar.Name (decompile CC.Zero firstBody) (decompileRest rest))
        | P.Let(var, e1, e2) ->
            //todo: this needs to be handled better for curried functions
            applyParens OP.Let (sprintf "let%s%s = %s in %s" (if var.IsMutable then " mutable " else " ") var.Name (decompile CC.Zero e1) (decompile CC.Zero e2))
        | P.QuoteTyped(qx) ->
            sprintf "<@ %s @>" (decompile CC.Zero qx)
        | P.QuoteRaw(qx) ->
            sprintf "<@@ %s @@>" (decompile CC.Zero qx)
        | DP.OrElse(DP.Bool(true), DP.Bool(false)) -> //true || false can't be distinguished from true && true, yet is less likely an expression due to short-circuiting
            applyParens OP.LogicalAnd "true && true"
        | DP.AndAlso(DP.Bool(false), DP.Bool(true)) -> //false && true can't be distinguished from false || false, yet is less likely an expression due to short-circuiting
            applyParens OP.LogicalOr "false || false"
        | DP.AndAlso(a,b) -> //must come before if then else
            applyParens OP.LogicalAnd (sprintf "%s && %s" (decompile (OP.LogicalAnd, OP.Left) a) (decompile (OP.LogicalAnd,OP.Right) b))
        | DP.OrElse(a,b) -> //must come before if then else
            applyParens OP.LogicalOr (sprintf "%s || %s" (decompile (OP.LogicalOr,OP.Left) a) (decompile (OP.LogicalOr,OP.Right) b))
        | P.IfThenElse(a,b, DP.Unit) -> //syntax doesn't require else branch when it's nothing but unit
            applyParens OP.If (sprintf "if %s then %s" (decompile (OP.If,OP.Non) a) (decompile (OP.If,OP.Non) b))
        | P.IfThenElse(a,b,c) ->
            applyParens OP.If (sprintf "if %s then %s else %s" (decompile (OP.If,OP.Non) a) (decompile (OP.If,OP.Non) b) (decompile (OP.If,OP.Non) c))
        | P.VarSet(v, arg) ->
            //not sure what precedence should be, using precedence for < op
            applyParens OP.LessThanOp (sprintf "%s <- %s" v.Name (decompile CC.Zero arg))
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
            applyParens OP.If (sprintf "match %s with | %s -> true | _ -> false" (decompile (OP.If,OP.Non) target) ucMatch)
        | P.TryWith(tryBody, _, _, catchVar, catchBody) ->
            applyParens OP.Try (sprintf "try %s with %s -> %s" (decompile (OP.Try,OP.Non) tryBody) catchVar.Name (decompile (OP.Try,OP.Non) catchBody))
        | P.TryFinally(tryBody, finallyBody) ->
            applyParens OP.Try (sprintf "try %s finally %s" (decompile (OP.Try,OP.Non) tryBody) (decompile (OP.Try,OP.Non) finallyBody))
        | P.WhileLoop(condition,body) ->
            applyParens OP.While (sprintf "while %s do %s" (decompile (OP.While,OP.Non) condition) (decompile (OP.Try,OP.Non) body))
        | P.ForIntegerRangeLoop(var,rangeStart,rangeEnd,body) ->
            applyParens OP.For (sprintf "for %s in %s..%s do %s" var.Name (decompile CC.Zero rangeStart) (decompile CC.Zero rangeEnd) (decompile (OP.For,OP.Non) body))
        | P.TupleGet(tup, index) -> //issue 80: reverted to version 1.2.3 TupleGet sprinting as fallback when TupleLet fails
            let tupleMatch =
                Seq.init
                    (Microsoft.FSharp.Reflection.FSharpType.GetTupleElements(tup.Type).Length)
                    (fun i -> if i=index then (sprintf "t%i" (index+1)) else "_")

            sprintf "(let %s = %s in %s)"
                (tupleMatch |> String.concat ",")
                (decompile CC.Zero tup)
                (sprintf "t%i" (index+1))
        | _ ->
            sprintf "%A" (expr)
    and decompileArgs prec delimiter exprs =
        exprs |> List.map (decompile prec) |> String.concat delimiter
    and decompileTupledArgs =
        decompileArgs (OP.Comma,OP.Non) ", "
    and decompileCurriedArgs = //application of arguments to a function
        decompileArgs (OP.Application,OP.Non) " "
    and decompileSequencedArgs =
        decompileArgs (OP.Semicolon,OP.Non) "; "

    decompile CC.Zero expr

//-----operator precedence updated April 2011 with bitwise ops-----
//see OperatorPrecedenceedence.xlsx, not yet implemented.

//-----precedence-----
//note: http://stackoverflow.com/questions/4859151/help-me-understand-lambda-expression-precedence
//spec: http://research.microsoft.com/en-us/um/cambridge/projects/fsharp/manual/spec.html
//from spec:  Paren(token) pushed when (, begin, struct, sig, {, [, [|, or quote-op-left is encountered.
//custom operator precedence determined by first op in sequence: http://stackoverflow.com/questions/3347972/f-custom-operators-precedence
//precedence table: http://msdn.microsoft.com/en-us/library/dd233228.aspx
(*
    Operator or expression		Associativity		Comments

27  f<types>					Left				High-precedence type application; see §15.3

26  f(x)						Left				High-precedence application; see §15.2

25  .							Left

24  prefix-op					Left				Applies to prefix uses of these symbols (i.e. +OP -OP % %% & && !OP (except !=) ~OP)

23  "| rule"					Right				Pattern matching rules

22  "f x"                       Left
    "lazy x"
    "assert x"

21  **OP						Right

20  *OP /OP %OP					Left

19  -OP +OP						Left				Applies to infix uses of these symbols

18  :?							Not associative

17  ::							Right

16  ^OP							Right

15  &&& ||| ^^^ ~~~ <<< >>>     Left

14  !=OP <OP >OP = |OP &OP $	Left

13  :> :?>						Left (note: the spec said right, but is incorrect)

12  & &&						Left                Logical AND

11  or ||						Left                Logical OR

10  ,							Not associative

9   :=							Right

8   ->							Right

7   if							Not associative

6   function, fun, match, try	Not associative

5   let							Not associative

4   ;							Right

3   |							Left

2    when						Right

1    as							Right
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

(*
Compiled names for other symbolic operators are op_N1...Nn where N1 to Nn are the names for the characters as shown in the table below. For example, the symbolic identifier <* has the compiled name op_LessMultiply:

>    Greater

<    Less

+    Plus

-    Minus

*    Multiply

=    Equals

~    Twiddle

%    Percent

.    Dot

&    Amp

|    Bar

@    At

#    Hash

^    Hat

!    Bang

?    Qmark

/    Divide

.    Dot

:    Colon

(    LParen

,    Comma

)    RParen

[    LBrack

]    RBrack

*)

(*

4.4.1     Categorization of Symbolic Operators
The following symbolic-op tokens can be used to form prefix and infix expressions. The marker OP represents all symbolic-op tokens that begin with the indicated prefix, except for tokens that appear elsewhere in the table.



infix-or-prefix-op :=

    +,  -, +., -., %, &, &&



prefix-op :=

    infix-or-prefix-op

    ~ ~~ ~~~                   (and any repetitions of ~)

    !OP                  (except !=)



infix-op :=

    infix-or-prefix-op

    -OP +OP || <OP >OP = |OP &OP ^OP *OP /OP %OP != !=OP

                         (or any of these preceded by one or more ‘.’)

    :=

    ::

    $

    or

    ?

*)