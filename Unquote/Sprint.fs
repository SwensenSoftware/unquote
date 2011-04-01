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
open Swensen

type binOpAssoc =
    | Left
    | Right
    | Non

let binaryOps = 
    [
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
    "op_PipeLeft", ("<|", 13, Left)
    "op_PipeLeft2", ("<||", 13, Left)
    "op_PipeLeft3", ("<|||", 13, Left)
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
    //set ref cell
    "op_ColonEquals", (":=", 9, Right)
    ] |> Map.ofList

//future feature, support custom ops
///Match non-custom binary infix Call patterns.
///Must come before Call pattern.
let (|BinaryInfixCall|_|) = function
    | Call (_, mi, lhs::rhs::[]) ->
        match binaryOps |> Map.tryFind mi.Name with
        | Some op -> Some(op,lhs,rhs)
        | None -> None
    | _ -> None

let unaryOps = 
    [
    "op_UnaryPlus", "+"
    "op_UnaryNegation", "-"
    "op_LogicalNot", "~~~"
    "op_Dereference", "!"
    ] |> Map.ofList

//all unary ops have precedence of 9
let (|UnaryPrefixCall|_|) = function
    | Call (_, mi, arg::[]) ->
        match unaryOps |> Map.tryFind mi.Name with
        | Some(op) -> Some(op, arg)
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
    |> (function | Some _ -> true | None -> false)

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

//used by both sprintSig and sprint
let applyParensForPrecInContext context prec s = if prec > context then s else sprintf "(%s)" s

//the usefullness of this function makes me think to open up Sprint module (currently just added TypeExt with this feature)
///Sprint the F#-style type signature of the given Type.  Handles known type abbreviations,
///simple types, arbitrarily complex generic types (multiple parameters and nesting),
///lambdas, tuples, and arrays.
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
        | CompiledMatch @"[\.\+]?([^\.\+]*)$" [_;nameMatch] -> nameMatch.Value //short name
        | cleanName -> failwith "failed to lookup type display name from it's \"clean\" name: " + cleanName

    let rec sprintSig context (t:Type) =
        let applyParens = applyParensForPrecInContext context
        let cleanName, arrSig = 
            //if is generic type, then doesn't have FullName, need to use just Name
            match (if String.IsNullOrEmpty(t.FullName) then t.Name else t.FullName) with
            | CompiledMatch @"^([^`\[]*)`?.*?(\[[\[\],]*\])?$" [_;cleanNameMatch;arrSigMatch] -> //long name type encoding left of `, array encoding at end
                cleanNameMatch.Value, arrSigMatch.Value
            | _ -> 
                failwith ("failed to parse type name: " + t.FullName)

        match t.GetGenericArguments() with
        | args when args.Length = 0 -> 
            (displayName cleanName) + arrSig
        | args when cleanName = "System.Tuple" ->
            (applyParens (if arrSig.Length > 0 then 0 else 3) (sprintf "%s" (args |> Array.map (sprintSig 3) |> String.concat " * "))) +  arrSig
        | [|lhs;rhs|] when cleanName = "Microsoft.FSharp.Core.FSharpFunc" -> //right assoc, binding not as strong as tuples
            (applyParens (if arrSig.Length > 0 then 0 else 2) (sprintf "%s -> %s" (sprintSig 2 lhs) (sprintSig 1 rhs))) + arrSig            
        | args ->
            sprintf "%s<%s>%s" (displayName cleanName) (args |> Array.map (sprintSig 1) |> String.concat ", ") arrSig
    
    sprintSig 0

//If the method is not generic, returns true. If the function is generic,
//the current algorithm tests whether the type parameters are a subset of those
//type parameters which are supplied by method parameters or method return type.
///Determine whether the generic args for a call are inferable
let genericArgsInferable (mi:MethodInfo) = 
    (mi.IsGenericMethod |> not) ||
        let miDefinition = mi.GetGenericMethodDefinition()
        let needed = miDefinition.GetGenericArguments() |> Array.map(fun arg -> arg.Name) |> set 
        let inferable = 
            miDefinition.GetParameters() 
            |> Seq.append (Seq.singleton miDefinition.ReturnParameter)
            |> Seq.map 
                (fun p -> 
                    if p.ParameterType.IsGenericParameter then [|p.ParameterType|]
                    elif p.ParameterType.ContainsGenericParameters then p.ParameterType.GetGenericArguments()
                    else [||]) 
            |> Seq.concat
            |> Seq.map (fun t -> t.Name)
            |> set

        inferable.IsSupersetOf(needed)

let sprintGenericArgs (mi:MethodInfo) =
    sprintf "<%s>" (mi.GetGenericArguments() |> Seq.map sprintSig |> String.concat ", ")

///sprints the generic arguments of a call if definitely not inferable.
let sprintGenericArgsIfNotInferable (mi:MethodInfo) =
    if genericArgsInferable mi then ""
    else sprintGenericArgs mi

let isListUnionCase (uci:UnionCaseInfo) = 
    uci.DeclaringType.IsGenericType && uci.DeclaringType.GetGenericTypeDefinition() = typedefof<list<_>>

///Test whether the given MemberInfo instance represents an F# Generic Value rather than 
///a Unit argument function.  Assumes that the MemberInfo instance is for a .NET member taking zero arguments.
let isGenericValue =
    memoize //performance testing shows about 10% performance increase in SourceOpTests.``Call distinguishes between generic value Call and unit function Call`` using memoization 
        (fun (mi:MemberInfo) ->
            try
                let mOrV =
                    FSharpEntity.FromType(mi.DeclaringType).MembersOrValues
                    |> Seq.find (fun mOrV -> mOrV.CompiledName = mi.Name)

                not mOrV.Type.IsFunction
            with
            | :? System.NotSupportedException -> true) //for dynamic assemblies, just assume idiomatic generic value

//suprisingly, this is actually used twice.
///Test whether the Expr is a Var and equals the given Var property-wise
let varEqualsExpr (x:Var) = function
    | Var y -> x.Name = y.Name && x.Type = y.Type && x.IsMutable = y.IsMutable
    | _ -> false

///Test whether the given expression represents a tuple let binding: e.g. let x,y = 1,2.
///Must come before Let pattern and after IncompleteLambdaCall pattern.
let (|TupleLet|_|) x =
    //N.B. breaking out the two TupleLetStart variations allows us to using | pattern match with start and body binding.

    ///TupleLet start variation 1) let a = TupleGet(tupleProperty, index) in let b = TupleGet(tupleProperty, index) in ...
    let (|TupleLetStart1|_|) = function
        | (Let(_,TupleGet(body, _),_) as start) ->
            Some(start, body)
        | _ -> None

    ///TupleLet start variation 2) let patternInput = expression in let a = TupleGet(patternInput, index) in ...
    let (|TupleLetStart2|_|) = function
        //this is getting a little crazy, but it is the observed pattern, and pi = piAgain is a necessary restriction
        //so as to not have too wide a net.
        | Let(var, body, (Let(_,TupleGet(varAgain,_),_) as start)) when varEqualsExpr var varAgain ->
            Some(start,body)
        | _ -> None

    match x with
    | TupleLetStart1(start,body) | TupleLetStart2(start,body) ->
        let rec gather varIndexList = function
            | Let(var,TupleGet(_,index),next) ->
                gather ((var,index)::varIndexList) next
            | final -> 
                 //need to sort varIndexList since tuple let bindings in lambda expressions
                 //are order in forward order, but normal tuple let bindings in reverse order.
                (varIndexList |> List.sortBy snd), final
        
        let varIndexList, final = gather [] start

        let tupleLength =  
            let rec calcTupleLength (ty:Type) = 
                match ty.Name with
                | CompiledMatch(@"Tuple`([1-8])") [_;g] ->
                    match g.Value with
                    | Int(8) -> 7 + (calcTupleLength (ty.GetProperty("Rest").PropertyType))
                    | Int(len) -> len
                    | _ -> failwithf "unexpected match: %A" g.Value
                | _ -> failwithf "unexcepted Type Name: %s" ty.Name
            calcTupleLength body.Type

        let varList = 
            let rec fillInGaps i input output =
                match input with
                | [] -> 
                    (List.init (tupleLength - i) (fun _ -> None)) @ output //pad output with None when there are "_" bindings extending past length of input
                | (var,index)::tail -> 
                    if index = i then 
                        fillInGaps (i+1) tail (Some(var)::output)
                    else 
                        fillInGaps (i+1) input (None::output)
            fillInGaps 0 varIndexList [] |> List.rev

        Some(varList, body, final)
    | _ -> None

////need to check all args are reduced?
///Partial application and zero application of Lambda call (e.g. List.map (+), or id).
///Must come before Let and Lambdas patterns.
///Cases: 1) Let .. Lambdas .. Call
///       2) Lambdas .. Call
let (|IncompleteLambdaCall|_|) x =
    match x with
    | (Let _ | Lambda _) -> //this is definately not a complete lambda call
        let rec gatherLetBindings varsList bindingList = function
            | TupleLet(vars, binding, body) -> 
                gatherLetBindings ((vars |> List.choose id)::varsList) (binding::bindingList) body
            | Let(var, binding, body) -> 
                gatherLetBindings ([var]::varsList) (binding::bindingList) body
            | final -> 
                varsList |> List.rev, bindingList |> List.rev, final

        let varsList, bindingList, final = gatherLetBindings [] [] x

        match final with
        | Lambdas(lambdaVarsList, Call(None, mi, callArgs)) 
            //requiring all callArgs to be Vars is a temp cheat till we know how to deal with properties in call args
            when List.equalsWith varEqualsExpr ((varsList |> List.concat) @ (lambdaVarsList |> List.concat)) callArgs -> 
                Some(mi, bindingList)
        | _ -> None
    | _ -> None
                
//todo:
//  precedence applied to lhs of . not right, see skipped SourceOpTests
//  note: Dictionary<_,_> values are not sprinted as nicely as in FSI, consider using FSI style
let sprint expr =
    let rec sprint context expr =
        let applyParens = applyParensForPrecInContext context
        match expr with
        | Sequential(Sequential(lhs, Unit), rhs) ->
            //due to quirky nested structure which handles implicit unit return values
            //need to hack precedence / application of parenthisizes.  we give
            //lhs anecdotally higher precedence context of 10.
            applyParens 4 (sprintf "%s; %s" (sprint 10 lhs) (sprint 3 rhs))
        | Sequential(lhs, rhs) -> 
            applyParens 4 (sprintf "%s; %s" (sprint 4 lhs) (sprint 3 rhs))
        | Application(curry, last) -> //application of arguments to a lambda
            applyParens 20 (sprintf "%s %s" (sprint 19 curry) (sprint 20 last))
        //issue 25 and issue 23: the following "re-sugars" both partially applied and unapplied lambda call expressions
        //must come before Lambdas
        | IncompleteLambdaCall(mi, args) -> //assume lambdas are only part of modules.
            match binaryOps |> Map.tryFind mi.Name with
                | Some(symbol,_,_) -> 
                    let sprintedSymbol = sprintf "(%s)" symbol
                    match args.Length with
                    | 1 -> applyParens 20 (sprintf "%s %s" sprintedSymbol (sprintCurriedArgs args))
                    | 0 -> sprintedSymbol
                    | _ -> failwithf "partial applied binary op should only have 0 or 1 args but has more: %A" args
                | None ->
                    match unaryOps |> Map.tryFind mi.Name with
                    | Some(symbol) -> sprintf "(~%s)" symbol
                    | None -> 
                        let sprintFunction (mi:MethodInfo) =
                            if isOpenModule mi.DeclaringType then sourceName mi
                            else sprintf "%s.%s" (sourceName mi.DeclaringType) (sourceName mi)
                        if args.Length = 0 then sprintFunction mi //not sure what precedence should be
                        else applyParens 20 (sprintf "%s %s" (sprintFunction mi) (sprintCurriedArgs args))
        | Lambdas(vars, body) -> //addresses issue 27
            let sprintSingleVar (var:Var) = if var.Type = typeof<Unit> then "()" else var.Name
            let sprintedVars =
                vars
                |> List.map  
                    (function 
                        | [var] -> sprintSingleVar var 
                        | tupledVars -> sprintf "(%s)" (tupledVars |> List.map sprintSingleVar |> String.concat ", "))
                |> String.concat " "
            applyParens 6 (sprintf "fun %s -> %s" sprintedVars (sprint 0 body))
        | BinaryInfixCall((symbol, prec, assoc), lhs, rhs) -> //must come before Call pattern
            let lhsValue, rhsValue = 
                match assoc with
                | Left -> sprint (prec-1) lhs, sprint prec rhs
                | Right -> sprint prec lhs, sprint (prec-1) rhs
                | Non -> sprint prec lhs, sprint prec rhs
            applyParens prec (sprintf "%s %s %s" lhsValue symbol rhsValue)
        | UnaryPrefixCall(symbol, arg) -> //must come before Call pattern
            applyParens 22 (sprintf "%s%s" symbol (sprint 22 arg))
        | Call(Some(target), mi, args) -> //instance call
            //just assume instance members always have tupled args
            applyParens 20 (sprintf "%s.%s%s(%s)" (sprint 22 target) mi.Name (sprintGenericArgsIfNotInferable mi) (sprintTupledArgs args))
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
        | Call(None, mi, args) -> //static call (we assume F# functions are always static calls for simplicity)
            if FSharpType.IsModule mi.DeclaringType then //we assume static calls in modules are functions, for simplicity
                let methodName = sourceName mi
                                
                //if mi has generic args which can't be infered, need to sprint them.
                //if mi takes no arguments, then need to sprint "()", unless mi is an F# value, in which case we omit ()
                let sprintedArgs = 
                    sprintf "%s%s"
                        (if genericArgsInferable mi then "" else sprintGenericArgs mi) 
                        (if args.Length = 0 then 
                            if isGenericValue(mi) then ""
                            else "()"
                         else " " + sprintCurriedArgs args)
                
                if isOpenModule mi.DeclaringType then 
                    applyParens 20 (sprintf "%s%s" methodName sprintedArgs)
                else 
                    applyParens 20 (sprintf "%s.%s%s" (sourceName mi.DeclaringType) methodName sprintedArgs)
            else //assume static calls in non-modules are members for simplicity, also CompiledName same as SourceName
                applyParens 20 (sprintf "%s.%s%s(%s)" mi.DeclaringType.Name mi.Name (sprintGenericArgsIfNotInferable mi) (sprintTupledArgs args))
        | PropertyGet(Some(target), pi, args) -> //instance get
            match pi.Name, args with
            | CompiledMatch(@"^Item(\d*)?$") _, _ when pi.DeclaringType |> FSharpType.IsUnion ->
                //for UnionCaseTypeTests, require a op_Dynamic implementation
                sprintf "(%s?%s : %s)" (sprint 22 target) pi.Name (pi.PropertyType |> sprintSig)
            | _, [] -> sprintf "%s.%s" (sprint 22 target) pi.Name //also includes "Item" with zero args
            | "Item", _ -> sprintf "%s.[%s]" (sprint 22 target) (sprintTupledArgs args)
            | _, _ -> applyParens 20 (sprintf "%s.%s(%s)" (sprint 22 target) pi.Name (sprintTupledArgs args))
        | PropertyGet(None, pi, _) -> //static get (note: can't accept params)
            if isOpenModule pi.DeclaringType then 
                sprintf "%s" pi.Name
            else
                sprintf "%s.%s" pi.DeclaringType.Name pi.Name
        | FieldGet(Some(target), fi) ->
            sprintf "%s.%s" (sprint 22 target) fi.Name
        | FieldGet(None, fi) ->
            sprintf "%s.%s" fi.DeclaringType.Name fi.Name
        | Unit -> "()" //must come before Value pattern
        | Value(o, _) ->
            match o with
            | null -> "null"
            | _ -> sprintf "%A" o
        | NewTuple(args) -> //tuples have at least two elements
            args |> sprintTupledArgs |> sprintf "(%s)" //what is precedence? 10?
        | NewArray(_,args) ->
            args |> sprintSequencedArgs |> sprintf "[|%s|]"
        //list union cases more complex than normal union cases since need to consider
        //both cons infix operator and literal list constructions.
        | NewUnionCase(uci,args) when uci |> isListUnionCase ->
            if args = [] then
                "[]"
            else
                let rec isLiteralConstruction = function
                    | NewUnionCase(_, lhs::(NewUnionCase(_, []))::[]) -> true //e.g. _::_::...::[]
                    | NewUnionCase(_, lhs::rhs::[]) ->
                        match rhs with
                        | NewUnionCase _ -> isLiteralConstruction rhs //e.g. _::_::...
                        | _ -> false //e.g. _::_::x
                    | _ -> failwith "unexpected list union case"

                if expr |> isLiteralConstruction then
                    let rec sprintLiteralConstructionArgs = function
                        | NewUnionCase(_, lhs::(NewUnionCase(_, []))::[]) -> sprint 4 lhs
                        | NewUnionCase(_, lhs::rhs::[]) ->
                            sprintf "%s; %s" (sprint 4 lhs) (sprintLiteralConstructionArgs rhs)
                        | _ -> failwith "unexpected list union case"
                    sprintf "[%s]" (sprintLiteralConstructionArgs expr)
                else 
                    //would like to optimize somehow so isLiteralConstruction is not called with every recursive 
                    //sprint of non literal constructions.
                    match args with
                    | lhs::rhs::[] -> applyParens 15 (sprintf "%s::%s" (sprint 15 lhs) (sprint 14 rhs))
                    | _ -> failwithf "unexpected list union case: %A" expr
        | NewUnionCase(uci,args) -> //"typical union case construction"
            match args with
            | [] -> uci.Name
            | _ -> sprintf "%s(%s)" uci.Name (sprintTupledArgs args)
        | NewObject(ci, args) ->
            applyParens 20 (sprintf "new %s(%s)" (sprintSig ci.DeclaringType) (sprintTupledArgs args))
        | Coerce(target, _) ->
            //don't even "mention" anything about the coersion
            sprint context target
        | TupleLet(vars, e1, e2) ->
            //if any are mutable, they are all mutable
            let anyMutable = vars |> List.exists (function | Some(v) -> v.IsMutable | None -> false)
            let varNames = vars |> List.map (function | Some(v) -> v.Name | None -> "_")
            applyParens 5 (sprintf "let%s%s = %s in %s" (if anyMutable then " mutable " else " ") (varNames |> String.concat ", ") (sprint 0 e1) (sprint 0 e2))
        | Let(var, e1, e2) ->
            //todo: this needs to be handled better for curried functions
            applyParens 5 (sprintf "let%s%s = %s in %s" (if var.IsMutable then " mutable " else " ") var.Name (sprint 0 e1) (sprint 0 e2))
        | Quote(qx) -> //even though can't reduce due to UntypedEval() limitations
            //note, this only handles typed quotations
            sprintf "<@ %s @>" (sprint 0 qx) 
        | AndAlso(a,b) -> //must come before if then else
            applyParens 12 (sprintf "%s && %s" (sprint 11 a) (sprint 12 b))
        | OrElse(a,b) -> //must come before if then else
            applyParens 11 (sprintf "%s || %s" (sprint 10 a) (sprint 11 b))
        | IfThenElse(a,b,c) ->
            applyParens 7 (sprintf "if %s then %s else %s" (sprint 7 a) (sprint 7 b) (sprint 7 c))
        //we can't reduce any XXXSet expressions due to limitations of Expr.Eval()
        | VarSet(v, arg) ->
            //not sure what precedence should be, using precedence for < op
            applyParens 13 (sprintf "%s <- %s" v.Name (sprint 0 arg)) 
        | FieldSet(Some(target), fi, arg) ->
            applyParens 13 (sprintf "%s.%s <- %s" (sprint 22 target) fi.Name (sprint 0 arg))
        | FieldSet(None, fi, arg) ->
            applyParens 13 (sprintf "%s.%s <- %s" fi.DeclaringType.Name fi.Name (sprint 0 arg))
        //extremely verbose
        | UnionCaseTest(target, uci) ->
            let ucMatch =
                if uci |> isListUnionCase then
                    if uci.Name = "Empty" then "[]"
                    else "_::_" //"Cons"
                else
                    let len = uci.GetFields().Length
                    if len = 0 then
                        sprintf "%s" uci.Name
                    else
                        sprintf "%s(%s)" uci.Name ("_" |> Array.create len |> String.concat ",")

            //using same precedence as if, 7, for match xxx with
            applyParens 7 (sprintf "match %s with | %s -> true | _ -> false" (sprint 7 target) ucMatch)
        | _ -> 
            sprintf "%A" (expr)
    and sprintArgs prec delimiter exprs =
        exprs |> List.map (sprint prec) |> String.concat delimiter
    and sprintTupledArgs = 
        sprintArgs 10 ", "
    and sprintCurriedArgs = //application of arguments to a function
        sprintArgs 20 " "
    and sprintSequencedArgs =
        sprintArgs 4 "; "
    
    sprint 0 expr

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