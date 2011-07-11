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

//Based on Microsoft's FSharp.Core\prim-types.fs, Apache License, Version 2.0

///The purpose of these operator implementations is two fold 1) many F# operators do not include dynamic impls,
///so we must give them. 2) even those operators which are given dynamic impls do not perform well since they
///need to be accessed via reflection, so we give "native" impls here.
module internal Swensen.Unquote.DynamicOperators
open System
open System.Reflection

//N.B. there are 13 primitive numeric types and also bigint to consider

///aty is the arg type, bty is the return type, x is the arg
let invokeExplicitOpDynamic (aty:Type) (bty:Type) (x:obj) =
    let ameth = aty.GetMethods() |> Array.find (fun m -> m.Name = "op_Explicit" && ((m.GetParameters() |> Array.map(fun p -> p.ParameterType)) = [| aty |]) && m.ReturnType = bty)
    match ameth  with 
    | null -> raise (NotSupportedException ())
    | m -> m.Invoke(null,[| x |])

///name is the name of the method, aty is the type of the first arg, bty is the type of the second arg,
///x is the first arg, y is the second arg.
let invokeBinOpDynamic name (aty:Type) (bty:Type) (x:obj) (y:obj) =
    let ameth = aty.GetMethod(name,[| aty; bty |])
    let bmeth = if aty.Equals(bty) then null else bty.GetMethod(name,[| aty; bty |])
    match ameth,bmeth  with 
    | null, null -> raise (NotSupportedException ())
    | m,null | null,m -> m.Invoke(null,[| x; y |])
    | _ -> raise (NotSupportedException ())

let inline (|InvokeBinOpStatic|_|) (op:'a->'b->'a) (aty:Type, x:obj, y:obj) =
    if aty.Equals(typeof<'a>) then Some(op (unbox<'a> x) (unbox<'b> y))
    else None

let inline (|InvokeOptionalBinOpStatic|_|) (op:('a->'b->'a) option) (aty:Type, x:obj, y:obj) =
    match op with
    | Some(op) when aty.Equals(typeof<'a>) -> Some(op (unbox<'a> x) (unbox<'b> y))
    | _ -> None

let inline invokeBinOp 
    name 
    op1 op2 op3 op4 op5 op6 op7 op8 op9 op10 op11 
    op12 op13 op14 op15 //non-integral types and string types not always supported
    (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj =
    let dyn() = invokeBinOpDynamic name aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then
        match aty, x, y with
        | InvokeBinOpStatic op1 (r:sbyte) -> box r
        | InvokeBinOpStatic op2 (r:int16) -> box r 
        | InvokeBinOpStatic op3 (r:int32) -> box r
        | InvokeBinOpStatic op4 (r:int64) -> box r
        | InvokeBinOpStatic op5 (r:nativeint) -> box r
        | InvokeBinOpStatic op6 (r:byte) -> box r
        | InvokeBinOpStatic op7 (r:uint16) -> box r
        | InvokeBinOpStatic op8 (r:uint32) -> box r
        | InvokeBinOpStatic op9 (r:uint64) -> box r
        | InvokeBinOpStatic op10 (r:unativeint) -> box r
        | InvokeBinOpStatic op11 (r:bigint) -> box r
        | InvokeOptionalBinOpStatic op12 (r:float) -> box r
        | InvokeOptionalBinOpStatic op13 (r:float32) -> box r
        | InvokeOptionalBinOpStatic op14 (r:decimal) -> box r
        | InvokeOptionalBinOpStatic op15 (r:string) -> box r
        | _ -> dyn()
    else dyn()

let op_Addition = invokeBinOp "op_Addition" (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (Some(+)) (Some(+)) (Some(+)) (Some(+)) //dynamic impl is given in F# lib, but we implement ourselves for perf. gain
let op_Multiply = invokeBinOp "op_Multiply" (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (Some(*)) (Some(*)) (Some(*)) None
let op_Subtraction = invokeBinOp "op_Subtraction" (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (Some(-)) (Some(-)) (Some(-)) None
let op_Division = invokeBinOp "op_Division" (/) (/) (/) (/) (/) (/) (/) (/) (/) (/) (/) (Some(/)) (Some(/)) (Some(/)) None
let op_Modulus = invokeBinOp "op_Modulus" (%) (%) (%) (%) (%) (%) (%) (%) (%) (%) (%) (Some(%)) (Some(%)) (Some(%)) None

let op_BitwiseOr = invokeBinOp "op_BitwiseOr" (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) None None None None
let op_BitwiseAnd = invokeBinOp "op_BitwiseAnd" (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) None None None None
let op_ExclusiveOr = invokeBinOp "op_ExclusiveOr" (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) None None None None
let op_LeftShift = invokeBinOp "op_LeftShift" (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) None None None None
let op_RightShift = invokeBinOp "op_RightShift" (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) None None None None

let binOpLookup : System.Collections.Generic.IDictionary<string, (Type->Type->Type->obj->obj->obj)> = 
    dict
        [("op_Addition", op_Addition)
         ("op_Multiply", op_Multiply)
         ("op_Subtraction", op_Subtraction)
         ("op_Division", op_Division)
         ("op_Modulus",op_Modulus)
         ("op_BitwiseOr",op_BitwiseOr)
         ("op_BitwiseAnd",op_BitwiseAnd)
         ("op_ExclusiveOr",op_ExclusiveOr)
         ("op_LeftShift",op_LeftShift)
         ("op_RightShift",op_RightShift)]

///name is the name of the unary op method, aty is the arg type, x is the arg
let invokeUnaryOpDynamic name (aty:Type) (x:obj) =
    let ameth = aty.GetMethod(name,[| aty |])
    match ameth  with 
    | null -> raise (NotSupportedException ())
    | m -> m.Invoke(null,[| x |])

let inline (|InvokeUnaryOpStatic|_|) (op:'a->'a) (aty:Type, x:obj) =
    if aty.Equals(typeof<'a>) then Some(op (unbox<'a> x))
    else None

let inline (|InvokeOptionalUnaryOpStatic|_|) (op:('a->'a) option) (aty:Type, x:obj) =
    match op with
    | Some(op) when aty.Equals(typeof<'a>) -> Some(op (unbox<'a> x))
    | _ -> None

let inline invokeUnaryOp 
    name 
    op1 op2 op3 op4 op5 op6 op7 op8 op9 op10
    op11 op12 op13 op14 //unsigned types (which includes byte) not always supported
    (aty:Type) (bty:Type) (x:obj) : obj =
    let dyn() = invokeUnaryOpDynamic name aty x
    if aty.Equals(bty) then
        match aty, x with
        | InvokeUnaryOpStatic op1 (r:sbyte) -> box r
        | InvokeUnaryOpStatic op2 (r:int16) -> box r 
        | InvokeUnaryOpStatic op3 (r:int32) -> box r
        | InvokeUnaryOpStatic op4 (r:int64) -> box r
        | InvokeUnaryOpStatic op5 (r:nativeint) -> box r
        | InvokeUnaryOpStatic op6 (r:bigint) -> box r
        | InvokeUnaryOpStatic op7 (r:float) -> box r
        | InvokeUnaryOpStatic op8 (r:float32) -> box r
        | InvokeUnaryOpStatic op9 (r:decimal) -> box r
        | InvokeOptionalUnaryOpStatic op10 (r:byte) -> box r
        | InvokeOptionalUnaryOpStatic op11 (r:uint16) -> box r
        | InvokeOptionalUnaryOpStatic op12 (r:uint32) -> box r
        | InvokeOptionalUnaryOpStatic op13 (r:uint64) -> box r
        | InvokeOptionalUnaryOpStatic op14 (r:unativeint) -> box r
        | _ -> dyn()
    else dyn()

let op_UnaryPlus = invokeUnaryOp "op_UnaryPlus" (~+) (~+) (~+) (~+) (~+) (~+) (~+) (~+) (~+) (Some(~+)) (Some(~+)) (Some(~+)) (Some(~+)) (Some(~+))
let op_UnaryNegation = invokeUnaryOp "op_UnaryNegation" (~-) (~-) (~-) (~-) (~-) (~-) (~-) (~-) (~-) None None None None None

let ToByte (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (byte (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (byte (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (byte (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (byte (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (byte (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (byte (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (byte (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (byte (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (byte (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (byte (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (byte (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (byte (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (byte (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (byte (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToSByte (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (sbyte (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (sbyte (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (sbyte (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (sbyte (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (sbyte (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (sbyte (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (sbyte (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (sbyte (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (sbyte (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (sbyte (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (sbyte (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (sbyte (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (sbyte (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (sbyte (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToUInt16 (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (uint16 (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (uint16 (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (uint16 (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (uint16 (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (uint16 (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (uint16 (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (uint16 (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (uint16 (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (uint16 (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (uint16 (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (uint16 (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (uint16 (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (uint16 (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (uint16 (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToInt16 (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (int16 (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (int16 (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (int16 (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (int16 (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (int16 (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (int16 (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (int16 (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (int16 (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (int16 (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (int16 (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (int16 (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (int16 (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (int16 (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (int16 (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToUInt32 (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (uint32 (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (uint32 (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (uint32 (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (uint32 (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (uint32 (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (uint32 (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (uint32 (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (uint32 (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (uint32 (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (uint32 (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (uint32 (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (uint32 (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (uint32 (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (uint32 (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToInt32 (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (int32 (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (int32 (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (int32 (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (int32 (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (int32 (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (int32 (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (int32 (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (int32 (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (int32 (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (int32 (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (int32 (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (int32 (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (int32 (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (int32 (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToInt = ToInt32

let ToUInt64 (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (uint64 (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (uint64 (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (uint64 (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (uint64 (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (uint64 (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (uint64 (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (uint64 (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (uint64 (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (uint64 (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (uint64 (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (uint64 (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (uint64 (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (uint64 (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (uint64 (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToInt64 (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (int64 (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (int64 (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (int64 (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (int64 (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (int64 (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (int64 (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (int64 (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (int64 (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (int64 (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (int64 (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (int64 (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (int64 (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (int64 (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (int64 (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToSingle (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (float32 (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (float32 (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (float32 (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (float32 (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (float32 (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (float32 (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (float32 (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (float32 (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (float32 (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (float32 (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (float32 (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (float32 (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (float32 (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (float32 (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToDouble (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (float (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (float (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (float (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (float (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (float (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (float (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (float (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (float (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (float (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (float (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (float (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (float (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (float (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (float (unbox<byte> x))
    elif aty.Equals(typeof<decimal>)    then box (float (unbox<decimal> x)) //**
    else invokeExplicitOpDynamic aty bty x

let ToDecimal (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (decimal (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (decimal (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (decimal (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (decimal (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (decimal (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (decimal (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (decimal (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (decimal (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (decimal (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (decimal (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (decimal (unbox<uint16> x))
//    elif aty.Equals(typeof<char>)       then box (decimal (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (decimal (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (decimal (unbox<byte> x))
    elif aty.Equals(typeof<decimal>)    then box (decimal (unbox<decimal> x))
    else invokeExplicitOpDynamic aty bty x

let ToUIntPtr (aty:Type) (bty:Type) (x:obj) : obj =
//    if aty.Equals(typeof<string>)       then box (unativeint (unbox<string> x))
    if aty.Equals(typeof<float>)      then box (unativeint (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (unativeint (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (unativeint (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (unativeint (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (unativeint (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (unativeint (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (unativeint (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (unativeint (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (unativeint (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (unativeint (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (unativeint (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (unativeint (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (unativeint (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToIntPtr (aty:Type) (bty:Type) (x:obj) : obj =
//    if aty.Equals(typeof<string>)       then box (unativeint (unbox<string> x))
    if aty.Equals(typeof<float>)      then box (nativeint (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (nativeint (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (nativeint (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (nativeint (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (nativeint (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (nativeint (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (nativeint (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (nativeint (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (nativeint (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (nativeint (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (nativeint (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (nativeint (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (nativeint (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let ToChar (aty:Type) (bty:Type) (x:obj) : obj =
    if aty.Equals(typeof<string>)       then box (char (unbox<string> x))
    elif aty.Equals(typeof<float>)      then box (char (unbox<float> x))
    elif aty.Equals(typeof<float32>)    then box (char (unbox<float32> x))
    elif aty.Equals(typeof<int64>)      then box (char (unbox<int64> x))
    elif aty.Equals(typeof<int32>)      then box (char (unbox<int32> x))
    elif aty.Equals(typeof<int16>)      then box (char (unbox<int16> x))
    elif aty.Equals(typeof<nativeint>)  then box (char (unbox<nativeint> x))
    elif aty.Equals(typeof<sbyte>)      then box (char (unbox<sbyte> x))
    elif aty.Equals(typeof<uint64>)     then box (char (unbox<uint64> x))
    elif aty.Equals(typeof<uint32>)     then box (char (unbox<uint32> x))
    elif aty.Equals(typeof<uint16>)     then box (char (unbox<uint16> x))
    elif aty.Equals(typeof<char>)       then box (char (unbox<char> x))
    elif aty.Equals(typeof<unativeint>) then box (char (unbox<unativeint> x))
    elif aty.Equals(typeof<byte>)       then box (char (unbox<byte> x))
    else invokeExplicitOpDynamic aty bty x

let unaryOpLookup : System.Collections.Generic.IDictionary<string, (Type->Type->obj->obj)> = 
    dict
        [("op_UnaryNegation",op_UnaryNegation)
         ("op_UnaryPlus", op_UnaryPlus)
         ("ToByte", ToByte)
         ("ToSByte", ToSByte)
         ("ToUInt16", ToUInt16)
         ("ToInt16", ToInt16)
         ("ToUInt32", ToUInt32)
         ("ToInt32", ToInt32)
         ("ToInt", ToInt)
         ("ToUInt64", ToUInt64)
         ("ToInt64", ToInt64)
         ("ToSingle", ToSingle)
         ("ToDouble", ToDouble)
         ("ToDecimal", ToDecimal)
         ("ToUIntPtr", ToUIntPtr)
         ("ToIntPtr", ToIntPtr)
         ("ToChar", ToChar)]

module Checked =
    open Checked
    let op_Addition = invokeBinOp "op_Addition" (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (+) (Some(+)) (Some(+)) (Some(+)) (Some(+)) //dynamic impl is given in F# lib, but we implement ourselves for perf. gain
    let op_Multiply = invokeBinOp "op_Multiply" (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (*) (Some(*)) (Some(*)) (Some(*)) None
    let op_Subtraction = invokeBinOp "op_Subtraction" (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (-) (Some(-)) (Some(-)) (Some(-)) None

    let binOpLookup : System.Collections.Generic.IDictionary<string, (Type->Type->Type->obj->obj->obj)> = 
        dict
            [("op_Addition", op_Addition)
             ("op_Subtraction", op_Subtraction)
             ("op_Multiply", op_Multiply)]

    let op_UnaryNegation = invokeUnaryOp "op_UnaryNegation" (~-) (~-) (~-) (~-) (~-) (~-) (~-) (~-) (~-) None None None None None

    let ToByte (aty:Type) (bty:Type) (x:obj) : obj =
        if aty.Equals(typeof<string>)       then box (Checked.byte (unbox<string> x))
        elif aty.Equals(typeof<float>)      then box (Checked.byte (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.byte (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.byte (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.byte (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.byte (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.byte (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.byte (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.byte (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.byte (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.byte (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.byte (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.byte (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.byte (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let ToSByte (aty:Type) (bty:Type) (x:obj) : obj =
        if aty.Equals(typeof<string>)       then box (Checked.sbyte (unbox<string> x))
        elif aty.Equals(typeof<float>)      then box (Checked.sbyte (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.sbyte (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.sbyte (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.sbyte (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.sbyte (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.sbyte (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.sbyte (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.sbyte (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.sbyte (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.sbyte (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.sbyte (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.sbyte (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.sbyte (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let ToUInt16 (aty:Type) (bty:Type) (x:obj) : obj =
        if aty.Equals(typeof<string>)       then box (Checked.uint16 (unbox<string> x))
        elif aty.Equals(typeof<float>)      then box (Checked.uint16 (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.uint16 (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.uint16 (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.uint16 (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.uint16 (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.uint16 (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.uint16 (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.uint16 (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.uint16 (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.uint16 (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.uint16 (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.uint16 (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.uint16 (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let ToInt16 (aty:Type) (bty:Type) (x:obj) : obj =
        if aty.Equals(typeof<string>)       then box (Checked.int16 (unbox<string> x))
        elif aty.Equals(typeof<float>)      then box (Checked.int16 (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.int16 (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.int16 (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.int16 (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.int16 (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.int16 (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.int16 (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.int16 (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.int16 (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.int16 (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.int16 (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.int16 (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.int16 (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let ToUInt32 (aty:Type) (bty:Type) (x:obj) : obj =
        if aty.Equals(typeof<string>)       then box (Checked.uint32 (unbox<string> x))
        elif aty.Equals(typeof<float>)      then box (Checked.uint32 (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.uint32 (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.uint32 (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.uint32 (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.uint32 (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.uint32 (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.uint32 (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.uint32 (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.uint32 (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.uint32 (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.uint32 (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.uint32 (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.uint32 (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let ToInt32 (aty:Type) (bty:Type) (x:obj) : obj =
        if aty.Equals(typeof<string>)       then box (Checked.int32 (unbox<string> x))
        elif aty.Equals(typeof<float>)      then box (Checked.int32 (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.int32 (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.int32 (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.int32 (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.int32 (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.int32 (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.int32 (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.int32 (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.int32 (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.int32 (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.int32 (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.int32 (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.int32 (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let ToInt = ToInt32

    let ToUInt64 (aty:Type) (bty:Type) (x:obj) : obj =
        if aty.Equals(typeof<string>)       then box (Checked.uint64 (unbox<string> x))
        elif aty.Equals(typeof<float>)      then box (Checked.uint64 (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.uint64 (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.uint64 (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.uint64 (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.uint64 (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.uint64 (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.uint64 (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.uint64 (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.uint64 (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.uint64 (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.uint64 (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.uint64 (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.uint64 (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let ToInt64 (aty:Type) (bty:Type) (x:obj) : obj =
        if aty.Equals(typeof<string>)       then box (Checked.int64 (unbox<string> x))
        elif aty.Equals(typeof<float>)      then box (Checked.int64 (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.int64 (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.int64 (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.int64 (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.int64 (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.int64 (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.int64 (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.int64 (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.int64 (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.int64 (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.int64 (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.int64 (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.int64 (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x
    
    //N.B. no checked decimal, float, or float32

    let ToUIntPtr (aty:Type) (bty:Type) (x:obj) : obj =
    //    if aty.Equals(typeof<string>)       then box (Checked.unativeint (unbox<string> x))
        if aty.Equals(typeof<float>)      then box (Checked.unativeint (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.unativeint (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.unativeint (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.unativeint (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.unativeint (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.unativeint (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.unativeint (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.unativeint (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.unativeint (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.unativeint (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.unativeint (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.unativeint (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.unativeint (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let ToIntPtr (aty:Type) (bty:Type) (x:obj) : obj =
    //    if aty.Equals(typeof<string>)       then box (Checked.unativeint (unbox<string> x))
        if aty.Equals(typeof<float>)      then box (Checked.nativeint (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.nativeint (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.nativeint (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.nativeint (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.nativeint (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.nativeint (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.nativeint (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.nativeint (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.nativeint (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.nativeint (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.nativeint (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.nativeint (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.nativeint (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let ToChar (aty:Type) (bty:Type) (x:obj) : obj =
        if aty.Equals(typeof<string>)       then box (Checked.char (unbox<string> x))
        elif aty.Equals(typeof<float>)      then box (Checked.char (unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (Checked.char (unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (Checked.char (unbox<int64> x))
        elif aty.Equals(typeof<int32>)      then box (Checked.char (unbox<int32> x))
        elif aty.Equals(typeof<int16>)      then box (Checked.char (unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (Checked.char (unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (Checked.char (unbox<sbyte> x))
        elif aty.Equals(typeof<uint64>)     then box (Checked.char (unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box (Checked.char (unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box (Checked.char (unbox<uint16> x))
        elif aty.Equals(typeof<char>)       then box (Checked.char (unbox<char> x))
        elif aty.Equals(typeof<unativeint>) then box (Checked.char (unbox<unativeint> x))
        elif aty.Equals(typeof<byte>)       then box (Checked.char (unbox<byte> x))
        else invokeExplicitOpDynamic aty bty x

    let unaryOpLookup : System.Collections.Generic.IDictionary<string, (Type->Type->obj->obj)> = 
        dict
            [("op_UnaryNegation",op_UnaryNegation)
             ("ToByte", ToByte)
             ("ToSByte", ToSByte)
             ("ToUInt16", ToUInt16)
             ("ToInt16", ToInt16)
             ("ToUInt32", ToUInt32)
             ("ToInt32", ToInt32)
             ("ToInt", ToInt)
             ("ToUInt64", ToUInt64)
             ("ToInt64", ToInt64)
             //N.B. not checked decimal, float, or float32
             ("ToUIntPtr", ToUIntPtr)
             ("ToIntPtr", ToIntPtr)
             ("ToChar", ToChar)]

    //N.B. not all ops have checked versions