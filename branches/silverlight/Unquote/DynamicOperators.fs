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

///name is the name of the method, aty is the type of the first arg, bty is the type of the second arg,
///x is the first arg, y is the second arg.
let invokeBinOpDynamic name (aty:Type) (bty:Type) (x:obj) (y:obj) =
    let ameth = aty.GetMethod(name,[| aty; bty |])
    let bmeth = if aty.Equals(bty) then null else bty.GetMethod(name,[| aty; bty |])
    match ameth,bmeth  with 
    | null, null -> raise (NotSupportedException ())
    | m,null | null,m -> m.Invoke(null,[| x; y |])
    | _ -> raise (NotSupportedException ())

let inline (|InvokeBinOpStatic|_|) (op:'a->'a->'a) (aty:Type, x:obj, y:obj) =
    if aty.Equals(typeof<'a>) then Some(op (unbox<'a> x) (unbox<'a> y))
    else None

let inline (|InvokeOptionalBinOpStatic|_|) (op:('a->'a->'a) option) (aty:Type, x:obj, y:obj) =
    match op with
    | Some(op) when aty.Equals(typeof<'a>) -> Some(op (unbox<'a> x) (unbox<'a> y))
    | _ -> None

let inline invokeBinOp 
    name 
    op1 op2 op3 op4 op5 op6 op7 op8 op9 op10 op11 
    op12 op13 op14 op15 //non-integral types and string types not always supported
    (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj =
    let dyn() = invokeBinOpDynamic name aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then //NOT TRUE FOR EXPLICIT OPS
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

let inline (|InvokeShiftBinOpStatic|_|) (op:'a->int->'a) (aty:Type, x:obj, y:obj) =
    if aty.Equals(typeof<'a>) then Some(op (unbox<'a> x) (unbox<int> y))
    else None

///Binary ops of the form 'a->int->'a
let inline invokeShiftBinOp 
    name 
    op1 op2 op3 op4 op5 op6 op7 op8 op9 op10 op11 
    (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj =
    let dyn() = invokeBinOpDynamic name aty bty x y //invokeBinOpDynamic works fine both for invokeBinOp and invokeShiftBinOp
    if aty.Equals(cty) && bty.Equals(typeof<int>) then
        match aty, x, y with
        | InvokeShiftBinOpStatic op1 (r:sbyte) -> box r
        | InvokeShiftBinOpStatic op2 (r:int16) -> box r 
        | InvokeShiftBinOpStatic op3 (r:int32) -> box r
        | InvokeShiftBinOpStatic op4 (r:int64) -> box r
        | InvokeShiftBinOpStatic op5 (r:nativeint) -> box r
        | InvokeShiftBinOpStatic op6 (r:byte) -> box r
        | InvokeShiftBinOpStatic op7 (r:uint16) -> box r
        | InvokeShiftBinOpStatic op8 (r:uint32) -> box r
        | InvokeShiftBinOpStatic op9 (r:uint64) -> box r
        | InvokeShiftBinOpStatic op10 (r:unativeint) -> box r
        | InvokeShiftBinOpStatic op11 (r:bigint) -> box r //even though this isn't an inlined IL call in prim-types, it is still statically resolved so better than reflection
        | _ -> dyn()
    else dyn()

let op_LeftShift = invokeShiftBinOp "op_LeftShift" (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<) (<<<)
let op_RightShift = invokeShiftBinOp "op_RightShift" (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>) (>>>)

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
    op1 op2 op3 op4 op5 op6 
    op7 op8 op9 op10 //non-integral types not supported by LogicalNot
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
        | InvokeOptionalUnaryOpStatic op6 (r:bigint) -> box r
        | InvokeOptionalUnaryOpStatic op7 (r:float) -> box r
        | InvokeOptionalUnaryOpStatic op8 (r:float32) -> box r
        | InvokeOptionalUnaryOpStatic op9 (r:decimal) -> box r
        | InvokeOptionalUnaryOpStatic op10 (r:byte) -> box r
        | InvokeOptionalUnaryOpStatic op11 (r:uint16) -> box r
        | InvokeOptionalUnaryOpStatic op12 (r:uint32) -> box r
        | InvokeOptionalUnaryOpStatic op13 (r:uint64) -> box r
        | InvokeOptionalUnaryOpStatic op14 (r:unativeint) -> box r
        | _ -> dyn()
    else dyn()

let op_UnaryPlus = invokeUnaryOp "op_UnaryPlus" (~+) (~+) (~+) (~+) (~+) (Some(~+)) (Some(~+)) (Some(~+)) (Some(~+)) (Some(~+)) (Some(~+)) (Some(~+)) (Some(~+)) (Some(~+))
let op_UnaryNegation = invokeUnaryOp "op_UnaryNegation" (~-) (~-) (~-) (~-) (~-) (Some(~-)) (Some(~-)) (Some(~-)) (Some(~-)) None None None None None
let op_LogicalNot = invokeUnaryOp "op_LogicalNot" (~~~) (~~~) (~~~) (~~~) (~~~) None None None None (Some(~~~)) (Some(~~~)) (Some(~~~)) (Some(~~~)) (Some(~~~))

///aty is the arg type, bty is the return type, x is the arg
let invokeExplicitOpDynamic (aty:Type) (bty:Type) (x:obj) =
    let ameth = aty.GetMethods() |> Array.find (fun m -> m.Name = "op_Explicit" && ((m.GetParameters() |> Array.map(fun p -> p.ParameterType)) = [| aty |]) && m.ReturnType = bty)
    match ameth  with 
    | null -> raise (NotSupportedException ())
    | m -> m.Invoke(null,[| x |])

let inline (|InvokeExplicitOpStatic|_|) (op:'a->'b) (aty:Type, x:obj) =
    if aty.Equals(typeof<'a>) then Some(op (unbox<'a> x))
    else None

let inline (|InvokeOptionalExplicitOpStatic|_|) (op:('a->'b) option) (aty:Type, x:obj) =
    match op with
    | Some(op) when aty.Equals(typeof<'a>) -> Some(op (unbox<'a> x))
    | _ -> None

let inline invokeExplicitOp //string and decimal are optional
    op1 op2 op3 op4 op5 op6 op7 op8 op9 op10 op11 op12 op13 op14 
    op15 op16 //string and char not always supported
    (aty:Type) (bty:Type) (x:obj) : obj =
    let dyn() = invokeExplicitOpDynamic aty bty x
    match aty, x with //note: writing the op type as <type>->'a instead of <type>->_ gives us extra static type checking in partial applications to help prevent typos
    | InvokeExplicitOpStatic (op1:sbyte->'a) r -> box r
    | InvokeExplicitOpStatic (op2:int16->'a) r -> box r 
    | InvokeExplicitOpStatic (op3:int32->'a) r -> box r
    | InvokeExplicitOpStatic (op4:int64->'a) r -> box r
    | InvokeExplicitOpStatic (op5:nativeint->'a) r -> box r
    | InvokeExplicitOpStatic (op6:float->'a) r -> box r
    | InvokeExplicitOpStatic (op7:float32->'a) r -> box r
    | InvokeExplicitOpStatic (op8:byte->'a) r -> box r
    | InvokeExplicitOpStatic (op9:uint16->'a) r -> box r
    | InvokeExplicitOpStatic (op10:uint32->'a) r -> box r
    | InvokeExplicitOpStatic (op11:uint64->'a) r -> box r
    | InvokeExplicitOpStatic (op12:unativeint->'a) r -> box r
    | InvokeOptionalExplicitOpStatic (op13:(bigint->'a) option) r -> box r
    | InvokeOptionalExplicitOpStatic (op14:(decimal->'a) option) r -> box r
    | InvokeOptionalExplicitOpStatic (op15:(string->'a) option) r -> box r
    | InvokeOptionalExplicitOpStatic (op16:(char->'a) option) r -> box r
    | _ -> dyn()

let ToByte = invokeExplicitOp byte byte byte byte byte byte byte byte byte byte byte byte (Some(byte)) (Some(byte)) (Some(byte)) (Some(byte))
let ToSByte = invokeExplicitOp sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte (Some(sbyte)) (Some(sbyte)) (Some(sbyte)) (Some(sbyte))
let ToUInt16 = invokeExplicitOp uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 (Some(uint16)) (Some(uint16)) (Some(uint16)) (Some(uint16))
let ToInt16 = invokeExplicitOp int16 int16 int16 int16 int16 int16 int16 int16 int16 int16 int16 int16 (Some(int16)) (Some(int16)) (Some(int16)) (Some(int16))
let ToUInt32 = invokeExplicitOp uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 (Some(uint32)) (Some(uint32)) (Some(uint32)) (Some(uint32))
let ToInt32 = invokeExplicitOp int32 int32 int32 int32 int32 int32 int32 int32 int32 int32 int32 int32 (Some(int32)) (Some(int32)) (Some(int32)) (Some(int32))
let ToInt = ToInt32
let ToUInt64 = invokeExplicitOp uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 (Some(uint64)) (Some(uint64)) (Some(uint64)) (Some(uint64))
let ToInt64 = invokeExplicitOp int64 int64 int64 int64 int64 int64 int64 int64 int64 int64 int64 int64 (Some(int64)) (Some(int64)) (Some(int64)) (Some(int64))
let ToSingle = invokeExplicitOp float32 float32 float32 float32 float32 float32 float32 float32 float32 float32 float32 float32 (Some(float32)) (Some(float32)) (Some(float32)) (Some(float32))
let ToDouble = invokeExplicitOp float float float float float float float float float float float float (Some(float)) (Some(float)) (Some(float)) (Some(float))
let ToDecimal = invokeExplicitOp decimal decimal decimal decimal decimal decimal decimal decimal decimal decimal decimal decimal (Some(decimal)) (Some(decimal)) (Some(decimal)) None
let ToUIntPtr = invokeExplicitOp unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint None None None (Some(unativeint))
let ToIntPtr = invokeExplicitOp nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint None None None (Some(nativeint))
let ToChar = invokeExplicitOp char char char char char char char char char char char char None (Some(char)) (Some(char)) (Some(char))

let unaryOpLookup : System.Collections.Generic.IDictionary<string, (Type->Type->obj->obj)> = 
    dict
        [("op_UnaryNegation",op_UnaryNegation)
         ("op_UnaryPlus", op_UnaryPlus)
         ("op_LogicalNot", op_LogicalNot)
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

    let op_UnaryNegation = invokeUnaryOp "op_UnaryNegation" (~-) (~-) (~-) (~-) (~-) (Some(~-)) (Some(~-)) (Some(~-)) (Some(~-)) None None None None None

    let ToByte = invokeExplicitOp byte byte byte byte byte byte byte byte byte byte byte byte (Some(byte)) (Some(byte)) (Some(byte)) (Some(byte))
    let ToSByte = invokeExplicitOp sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte sbyte (Some(sbyte)) (Some(sbyte)) (Some(sbyte)) (Some(sbyte))
    let ToUInt16 = invokeExplicitOp uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 uint16 (Some(uint16)) (Some(uint16)) (Some(uint16)) (Some(uint16))
    let ToInt16 = invokeExplicitOp int16 int16 int16 int16 int16 int16 int16 int16 int16 int16 int16 int16 (Some(int16)) (Some(int16)) (Some(int16)) (Some(int16))
    let ToUInt32 = invokeExplicitOp uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 uint32 (Some(uint32)) (Some(uint32)) (Some(uint32)) (Some(uint32))
    let ToInt32 = invokeExplicitOp int32 int32 int32 int32 int32 int32 int32 int32 int32 int32 int32 int32 (Some(int32)) (Some(int32)) (Some(int32)) (Some(int32))
    let ToInt = ToInt32
    let ToUInt64 = invokeExplicitOp uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 uint64 (Some(uint64)) (Some(uint64)) (Some(uint64)) (Some(uint64))
    let ToInt64 = invokeExplicitOp int64 int64 int64 int64 int64 int64 int64 int64 int64 int64 int64 int64 (Some(int64)) (Some(int64)) (Some(int64)) (Some(int64))
    //N.B. no checked decimal, float, or float32
    let ToUIntPtr = invokeExplicitOp unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint unativeint None None None (Some(unativeint))
    let ToIntPtr = invokeExplicitOp nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint nativeint None None None (Some(nativeint))
    let ToChar = invokeExplicitOp char char char char char char char char char char char char None (Some(char)) (Some(char)) (Some(char))

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