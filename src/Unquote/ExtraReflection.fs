///Extra reflection functions sprinting and reducing Quotation Expressions
module internal Swensen.Unquote.ExtraReflection
open System
open System.Reflection
open Microsoft.FSharp.Reflection

open Swensen.Utils
module OP = OperatorPrecedence

open System
open System.Text.RegularExpressions

module SymbolicOps =
    type symbolicOp =
        //bool: requires twiddle prefix
        | Prefix of bool
        //OP.OperatorPrecedence : op precedence
        | Infix of OP.OperatorPrecedence
        //neither prefix nor infix
        | FirstClassOnly

    let private symbolicOps = //to do, add op_Dollar to this table
        [   //boolean ops
            "op_Equality", ("=", Infix(OP.EqualsOp))
            "op_GreaterThan", (">", Infix(OP.GreaterThanOp))
            "op_LessThan", ("<", Infix(OP.LessThanOp))
            "op_GreaterThanOrEqual", (">=", Infix(OP.GreaterThanOp))
            "op_LessThanOrEqual", ("<=", Infix(OP.LessThanOp))
            "op_Inequality", ("<>", Infix(OP.LessThanOp))
            //pipe ops
            "op_PipeRight", ("|>", Infix(OP.PipeOp))
            "op_PipeRight2", ("||>", Infix(OP.Pipe))
            "op_PipeRight3", ("|||>", Infix(OP.Pipe))
            "op_PipeLeft", ("<|", Infix(OP.LessThanOp))
            "op_PipeLeft2", ("<||", Infix(OP.LessThanOp))
            "op_PipeLeft3", ("<|||", Infix(OP.LessThanOp))
            //numeric ops
            "op_Addition", ("+", Infix(OP.PlusBinaryOp))
            "op_Subtraction", ("-", Infix(OP.MinusBinaryOp))
            "op_Division", ("/", Infix(OP.DivideOp))
            "op_Multiply", ("*", Infix(OP.MultiplyOp))
            "op_Modulus", ("%", Infix(OP.ModOp))
            "op_Exponentiation", ("**", Infix(OP.ExponentiationOp))
            //bit operators
            "op_BitwiseAnd", ("&&&", Infix(OP.BitwiseAnd))
            "op_BitwiseOr", ("|||", Infix(OP.BitwiseOr))
            "op_ExclusiveOr", ("^^^", Infix(OP.ExclusiveOr))
            "op_LeftShift", ("<<<", Infix(OP.LeftShift))
            "op_RightShift", (">>>", Infix(OP.RightShift))

            //composition
            "op_ComposeRight", (">>", Infix(OP.GreaterThanOp))
            "op_ComposeLeft", ("<<", Infix(OP.LessThanOp))
            //special
            "op_Append", ("@", Infix(OP.AppendOp)) //not sure what precedence, falling back on (+))
            "op_Concatenate", ("^", Infix(OP.ConcatenateOp)) //ocaml style string concatentation
            //set ref cell
            "op_ColonEquals", (":=", Infix(OP.RefAssign))

            //op assign operators
            "op_AdditionAssignment", ("+=", Infix(OP.PlusBinaryOp))
            "op_SubtractionAssignment", ("-=", Infix(OP.MinusBinaryOp))
            "op_MultiplyAssignment", ("*=", Infix(OP.MultiplyOp))
            "op_DivisionAssignment", ("/=", Infix(OP.DivideOp))

            //"", ("",)
            //some more exotic operators
            "op_BooleanOr", ("||", Infix(OP.LogicalOr))
            //we decline to support the "or" infix operator since it doesn't follow "op_" prefix naming convention and thus may lead to ambiguity
            "op_BooleanAnd", ("&&", Infix(OP.LogicalAnd))
            "op_Amp", ("&", Infix(OP.LogicalAnd))
            "op_Dollar", ("$", Infix(OP.DollarOp))

            //require leading twidle in first-class use
            "op_UnaryPlus", ("+", Prefix(true))
            "op_UnaryNegation", ("-", Prefix(true))
            "op_Splice", ("%", Prefix(true))
            "op_SpliceUntyped", ("%%", Prefix(true))
            "op_AddressOf", ("&", Prefix(true))
            "op_IntegerAddressOf", ("&&", Prefix(true))
            "op_TwiddlePlusDot", ("+.", Prefix(true))
            "op_TwiddleMinusDot", ("-.", Prefix(true))

            //don't require leading twidle in first-class use
            "op_LogicalNot", ("~~~", Prefix(false))
            "op_Dereference", ("!", Prefix(false))

            //first class only operators
            "op_Range", ("..", FirstClassOnly)
            "op_RangeStep", (".. ..", FirstClassOnly)
            "op_Quotation", ("<@ @>", FirstClassOnly)
            "op_QuotationUntyped", ("<@@ @@>", FirstClassOnly)
        ] |> Map.ofList

    let private customOpParts =
        [
            "Greater", (">", Some(OP.GreaterThanOp))
            "Less", ("<", Some(OP.LessThanOp))
            "Plus", ("+", Some(OP.PlusBinaryOp))
            "Minus", ("-", Some(OP.MinusBinaryOp))
            "Multiply", ("*", Some(OP.MultiplyOp))
            "Equals", ("=", Some(OP.EqualsOp))
            "Twiddle", ("~", None)
            "Percent", ("%", Some(OP.ModOp))
            "Dot", (".", None)
            "Amp", ("&", Some(OP.AndOp))
            "Bar", ("|", Some(OP.PipeOp))
            "At", ("@", None)
            "Hash", ("#", None)
            "Hat", ("^", Some(OP.ConcatenateOp))
            "Bang", ("!", Some(OP.BangEqualsOp)) //i.e. !=OP infix sequence (!OP is infix only)
            "Qmark", ("?", None)
            "Divide", ("/", Some(OP.DivideOp))
            "Dot", (".", None)
            "Colon", (":", None)
            "LParen", ("(", None)
            "Comma", (",", None)
            "RParen", (")", None)
            "LBrack", ("[", None)
            "RBrack", ("]", None)
        ] |> Map.ofList

    let private customOpPattern = customOpParts |> Seq.map (fun kvp -> kvp.Key) |> String.concat "|"

    let tryFindByName name =
        match symbolicOps |> Map.tryFind name with
        | Some(op) -> Some(op)
        | None ->
            if name.StartsWith("op_") then
                let opSeq = name.Substring(3)

                let opSeqMatches = Regex.Matches(opSeq, customOpPattern)
                let opSeqMatchValues = opSeqMatches |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Seq.toList
                if opSeqMatchValues |> Seq.sumBy String.length = opSeq.Length then //this is an op_XXX sequence we recognize
                    let opSeqParts = opSeqMatchValues |> Seq.map (fun opName -> customOpParts |> Map.find opName) |> Seq.toList
                    let opInfo =
                        match opSeqParts with
                        | ("~",_)::_ -> Prefix(false)
                        | ("!",_)::(trailingOp,_)::_ when trailingOp <> "=" -> Prefix(false)
                        | _ ->
                            opSeqParts
                            |> Seq.find (fun (opSymbol, _ )-> opSymbol <> ".") //danger!
                            |> snd
                            |> Option.get //danger!
                            |> symbolicOp.Infix

                    let opSymbol = opSeqParts |> Seq.map fst |> String.concat ""
                    Some(opSymbol, opInfo)
                else
                    None
            else
                None

    ///try to find the first class symbolic function representation of a "op_" function name
    let tryMapAsFirstClassByName name =
        match tryFindByName name with
        | Some(symbol, Prefix(true)) ->
            Some(sprintf "(~%s)" symbol)
        | Some(symbol:string, _) ->
            if (symbol:string).StartsWith("*") || symbol.EndsWith("*") then
                Some(sprintf "( %s )" symbol)
            else
                Some(sprintf "(%s)" symbol)
        | _ -> None

let inline isGenericTypeDefinedFrom<'a> (ty:Type) =
    ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<'a>

///is the top-level FSI module
let inline isFsiModule (declaringType:Type) =
    declaringType.Name.StartsWith("FSI_")

//maybe consider something involving System.Diagnostics.StackFrame(false).GetMethod() to help out more.
//best we can seem to do
let isOpenModule (declaringType:Type) =
    isFsiModule declaringType ||
    declaringType.GetCustomAttributes(true)
    |> Array.tryFind (function | :? AutoOpenAttribute -> true | _ -> false)
    |> (function | Some _ -> true | None -> false)

let sourceNameFromString =
    let isActivePattern (name: string) = name.StartsWith("|") && name.EndsWith("|")

    let specialChars = @"~!@#$%^&*()+"".,:;<>\/?{}[]`|".ToCharArray()

    let reservedWords =
      set
        ["atomic"; "break"; "checked"; "component"; "const"; "constraint";
         "constructor"; "continue"; "eager"; "fixed"; "fori"; "functor"; "include";
         "measure"; "method"; "mixin"; "object"; "parallel"; "params"; "process";
         "protected"; "pure"; "recursive"; "sealed"; "tailcall"; "trait";
         "virtual"; "volatile"]

    let keywords =
      set
        ["abstract"; "and"; "as"; "assert"; "base"; "begin"; "class"; "default";
         "delegate"; "do"; "done"; "downcast"; "downto"; "elif"; "else"; "end";
         "exception"; "extern"; "false"; "finally"; "for"; "fun"; "function";
         "global"; "if"; "in"; "inherit"; "inline"; "interface"; "internal";
         "lazy"; "let"; "match"; "member"; "module"; "mutable"; "namespace"; "new";
         "null"; "of"; "open"; "or"; "override"; "private"; "public"; "rec";
         "return"; "sig"; "static"; "struct"; "then"; "to"; "true"; "try"; "type";
         "upcast"; "use"; "val"; "void"; "when"; "while"; "with"; "yield"]


    let hasSpecialChars (name: string) = name.IndexOfAny(specialChars) <> -1
    let isReservedWord name = reservedWords |> Set.contains name
    let isKeyword name = keywords |> Set.contains name

    let parenthesize name = sprintf "(%s)" name
    let escape name = sprintf "``%s``" name

    fun name ->
        if name |> isActivePattern then //n.b. active patterns can't have escaped names
            parenthesize name
        elif name |> hasSpecialChars || name |> isReservedWord || name |> isKeyword then //escape because has special char or is reserved word
            escape name
        else
            match SymbolicOps.tryMapAsFirstClassByName name with
            | Some(op) -> op
            | None -> name

///get the source name for the Module or F# Function represented by the given MemberInfo
let sourceName (mi:MemberInfo) =
    mi.GetCustomAttributes(true)
    |> Seq.tryPick
        (function
            | :? CompilationSourceNameAttribute as csna -> Some(csna.SourceName)
            | :? CompilationRepresentationAttribute as cra ->
                //seems sufficient, but may not be as robust as FSharpEntity.DisplayName
                if cra.Flags = CompilationRepresentationFlags.ModuleSuffix then
                    Some(mi.Name.Substring(0, mi.Name.Length - 6))
                else
                    None
            | _ -> None)
    |> function
    | Some sourceName -> sourceName
    | None ->
        match mi with
        // https://github.com/fsharp/fslang-design/blob/main/FSharp-4.1/FS-1019-implicitly-add-the-module-suffix.md
        | :? Type as t when FSharpType.IsModule t && t.Name.EndsWith "Module" ->
            // There is a type collision. Assume that the module and the type names overlapped causing an implicit 'Module' suffix to be added.
            match t.Assembly.GetType(t.FullName.Substring(0, t.FullName.Length - 6)) with
            | null -> mi.Name
            | _ -> t.Name.Substring(0, t.Name.Length - 6)
        | _ -> mi.Name
    |> sourceNameFromString //issue 11: active pattern function names need to be surrounded by parens

let inline private applyParensForPrecInContext context prec s = if prec > context then s else sprintf "(%s)" s

//the usefullness of this function makes me think to open up Sprint module (currently just added TypeExt with this feature)
///Sprint the F#-style type signature of the given Type.  Handles known type abbreviations,
///simple types, arbitrarily complex generic types (multiple parameters and nesting),
///lambdas, tuples, and arrays.
let sprintSig (outerTy:Type) =
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
        | "Microsoft.FSharp.Core.FSharpChoice"     -> "Choice"
        | "Microsoft.FSharp.Core.FSharpRef"     -> "ref"
        | "Microsoft.FSharp.Core.FSharpResult"     -> "Result"
        | "Microsoft.FSharp.Core.FSharpOption"  -> "option"
        | "Microsoft.FSharp.Core.FSharpValueOption"  -> "voption"
        | "Microsoft.FSharp.Control.FSharpHandler"  -> "Handler"
        | "Microsoft.FSharp.Collections.FSharpList" -> "list"
        | "Microsoft.FSharp.Collections.FSharpMap"  -> "Map"
        | "Microsoft.FSharp.Collections.FSharpSet"  -> "Set"
        | "System.Collections.Generic.IEnumerable"  -> "seq"
        | "System.Collections.Generic.List"  -> "ResizeArray"
        | Regex.Compiled.Match @"[\.\+]?([^\.\+]*)$" { GroupValues=[name] }-> name //short name
        | cleanName -> failwith "failed to lookup type display name from it's \"clean\" name: " + cleanName

    let rec sprintSig context (ty:Type) =
        let applyParens = applyParensForPrecInContext context
        let cleanName, arrSig =
            //if is generic type, then doesn't have FullName, need to use just Name
            match (if String.IsNullOrEmpty(ty.FullName) then ty.Name else ty.FullName) with
            | Regex.Compiled.Match @"^([^`\[]*)`?.*?(\[[\[\],]*\])?$" { GroupValues=[cleanName;arrSig] }-> //long name type encoding left of `, array encoding at end
                cleanName, arrSig
            | _ ->
                failwith ("failed to parse type name: " + ty.FullName)

        match ty.GetGenericArgumentsArrayInclusive() with
        | args when args.Length = 0 ->
            (if outerTy.IsGenericTypeDefinition then "'" else "") + (displayName cleanName) + arrSig
        | args when cleanName = "System.Tuple" && args.Length >= 2 ->
            (applyParens (if arrSig.Length > 0 then 0 else 3) (sprintf "%s" (args |> Array.map (sprintSig 3) |> String.concat " * "))) +  arrSig
        | args when cleanName = "System.ValueTuple" && args.Length >= 2 ->
            "struct" + (applyParens 0 (sprintf "%s" (args |> Array.map (sprintSig 3) |> String.concat " * "))) +  arrSig
        | [|lhs;rhs|] when cleanName = "Microsoft.FSharp.Core.FSharpFunc" -> //right assoc, binding not as strong as tuples
            (applyParens (if arrSig.Length > 0 then 0 else 2) (sprintf "%s -> %s" (sprintSig 2 lhs) (sprintSig 1 rhs))) + arrSig
        | args ->
            sprintf "%s<%s>%s" (displayName cleanName) (args |> Array.map (sprintSig 1) |> String.concat ", ") arrSig

    sprintSig 0 outerTy

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
let inline sprintGenericArgsIfNotInferable (mi:MethodInfo) =
    if genericArgsInferable mi then ""
    else sprintGenericArgs mi

let inline isListUnionCase (uci:UnionCaseInfo) =
    uci.DeclaringType |> isGenericTypeDefinedFrom<list<_>>

type fsharpValueType =
    | Function
    | GenericValue

let (|FunctionOrGenericValue|_|) (mi:MethodInfo) =
    //let fallback () =
    if FSharpType.IsModule mi.DeclaringType then
        if mi.GetParameters().Length = 0
           && (mi.IsGenericMethod && mi.GetGenericArguments().Length > 0)
           && not (mi.Name = "Reraise" && mi.DeclaringType.FullName = "Microsoft.FSharp.Core.Operators") then
            Some(GenericValue)
        else
            Some(Function)
    else None

//Issue 68: removing Metadata dependency, not worth it for this one scenario
//    try
//        let mOrV =
//            FSharpEntity.FromType(mi.DeclaringType).MembersOrValues
//            |> Seq.tryFind (fun mOrV -> mOrV.CompiledName = mi.Name)
//
//        match mOrV with
//        | Some(mOrV) when mOrV.Type.IsFunction -> Some(Function)
//        | Some(_) -> Some(GenericValue)
//        | None -> None
//    with
//    //PowerPack MetadataReader throws NotSupported Exception in dynamic assemblies like FSI
//    //and also more worrying it throws internal exceptions sometimes in other cases (should file bug!)
//    //so we need to take empirical guesses as to whether the given mi represents a Function or GenericValue
//    | :? System.NotSupportedException | _  ->
//        fallback ()