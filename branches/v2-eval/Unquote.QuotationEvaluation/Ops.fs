//based on Microsoft's Apache licensed F# 2.0 Operators.fs code

///The purpose of these operator implementations is two fold 1) many F# operators do not include dynamic impls,
///so we must give them. 2) even those operators which are given dynamic impls do not perform well since they
///need to be accessed via reflection, so we give "native" impls here.
module internal Swensen.Unquote.QuotationEvaluation.Ops
open System
open System.Reflection

///name is the name of the method, aty is the type of the first arg, bty is the type of the second arg,
///x is the first arg, y is the second arg.
let invokeBinOp name (aty:Type) (bty:Type) (x:obj) (y:obj) =
    let ameth = aty.GetMethod(name,[| aty; bty |])
    let bmeth = if aty.Equals(bty) then null else bty.GetMethod(name,[| aty; bty |])
    match ameth,bmeth  with 
    | null, null -> raise (NotSupportedException ())
    | m,null | null,m -> m.Invoke(null,[| x; y |])
    | _ -> raise (NotSupportedException ())

///name is the name of the unary op method, aty is the arg type, x is the arg
let invokeUnaryOp name (aty:Type) (x:obj) =
    let ameth = aty.GetMethod(name,[| aty |])
    match ameth  with 
    | null -> raise (NotSupportedException ())
    | m -> m.Invoke(null,[| x |])

///aty is the arg type, bty is the return type, x is the arg
let invokeExplicitOp (aty:Type) (bty:Type) (x:obj) =
    let ameth = aty.GetMethods() |> Array.find (fun m -> m.Name = "op_Explicit" && ((m.GetParameters() |> Array.map(fun p -> p.ParameterType)) = [| aty |]) && m.ReturnType = bty)
    match ameth  with 
    | null -> raise (NotSupportedException ())
    | m -> m.Invoke(null,[| x |])

//dynamic impl is given in F# lib, but we implement ourselves for perf. gain
let op_Addition (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_Addition" aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      + (unbox<sbyte> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      + (unbox<int16> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      + (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      + (unbox<int64> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  + (unbox<nativeint> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       + (unbox<byte> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     + (unbox<uint16> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     + (unbox<uint32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     + (unbox<uint64> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) + (unbox<unativeint> y))
        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      + (unbox<float> y))
        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    + (unbox<float32> y))
        elif aty.Equals(typeof<string>)     then box ((unbox<string> x)     + (unbox<string> y))
        elif aty.Equals(typeof<decimal>)    then box ((unbox<decimal> x)    + (unbox<decimal> y))
        elif aty.Equals(typeof<bigint>)     then box ((unbox<bigint> x)     + (unbox<bigint> y))
        else dyn()
    else dyn()

//dynamic impl is given in F# lib, but we implement ourselves for perf. gain
let op_Multiply (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_Multiply" aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      * (unbox<sbyte> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      * (unbox<int16> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      * (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      * (unbox<int64> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  * (unbox<nativeint> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       * (unbox<byte> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     * (unbox<uint16> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     * (unbox<uint32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     * (unbox<uint64> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) * (unbox<unativeint> y))
        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      * (unbox<float> y))
        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    * (unbox<float32> y))
        elif aty.Equals(typeof<decimal>)    then box ((unbox<decimal> x)    * (unbox<decimal> y))
        elif aty.Equals(typeof<bigint>)     then box ((unbox<bigint> x)      * (unbox<bigint> y))
        else dyn()
    else dyn()

let op_Subtraction (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_Subtraction" aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      - (unbox<sbyte> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      - (unbox<int16> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      - (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      - (unbox<int64> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  - (unbox<nativeint> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       - (unbox<byte> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     - (unbox<uint16> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     - (unbox<uint32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     - (unbox<uint64> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) - (unbox<unativeint> y))
        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      - (unbox<float> y))
        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    - (unbox<float32> y))
        elif aty.Equals(typeof<decimal>)    then box ((unbox<decimal> x)    - (unbox<decimal> y))
        elif aty.Equals(typeof<bigint>)     then box ((unbox<bigint> x)     - (unbox<bigint> y))
        else dyn()
    else dyn()

let op_Division (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_Division" aty bty x y                        
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      / (unbox<sbyte> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      / (unbox<int16> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      / (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      / (unbox<int64> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  / (unbox<nativeint> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       / (unbox<byte> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     / (unbox<uint16> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     / (unbox<uint32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     / (unbox<uint64> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) / (unbox<unativeint> y))
        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      / (unbox<float> y))
        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    / (unbox<float32> y))
        elif aty.Equals(typeof<decimal>)    then box ((unbox<decimal> x)    / (unbox<decimal> y))
        elif aty.Equals(typeof<bigint>)     then box ((unbox<bigint> x)     / (unbox<bigint> y))
        else dyn()
    else dyn()

let op_Modulus (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_Modulus" aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      % (unbox<sbyte> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      % (unbox<int16> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      % (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      % (unbox<int64> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  % (unbox<nativeint> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       % (unbox<byte> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     % (unbox<uint16> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     % (unbox<uint32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     % (unbox<uint64> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) % (unbox<unativeint> y))
        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      % (unbox<float> y))
        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    % (unbox<float32> y))
        elif aty.Equals(typeof<decimal>)    then box ((unbox<decimal> x)    % (unbox<decimal> y))
        elif aty.Equals(typeof<bigint>)     then box ((unbox<bigint> x)     % (unbox<bigint> y))
        else dyn()
    else dyn()

let op_BitwiseOr (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_BitwiseOr" aty bty x y                        
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      ||| (unbox<sbyte> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      ||| (unbox<int16> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      ||| (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      ||| (unbox<int64> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  ||| (unbox<nativeint> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       ||| (unbox<byte> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     ||| (unbox<uint16> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     ||| (unbox<uint32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     ||| (unbox<uint64> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) ||| (unbox<unativeint> y))
        elif aty.Equals(typeof<bigint>)     then box ((unbox<bigint> x)     ||| (unbox<bigint> y))
//        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      ||| (unbox<float> y))
//        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    ||| (unbox<float32> y))
        else dyn()
    else dyn()

let op_BitwiseAnd (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_BitwiseAnd" aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      &&& (unbox<sbyte> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      &&& (unbox<int16> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      &&& (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      &&& (unbox<int64> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  &&& (unbox<nativeint> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       &&& (unbox<byte> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     &&& (unbox<uint16> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     &&& (unbox<uint32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     &&& (unbox<uint64> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) &&& (unbox<unativeint> y))
        elif aty.Equals(typeof<bigint>)     then box ((unbox<bigint> x)     &&& (unbox<bigint> y))
//        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      &&& (unbox<float> y))
//        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    &&& (unbox<float32> y))
        else dyn()
    else dyn()

let op_ExclusiveOr (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_ExclusiveOr" aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      ^^^ (unbox<sbyte> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      ^^^ (unbox<int16> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      ^^^ (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      ^^^ (unbox<int64> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  ^^^ (unbox<nativeint> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       ^^^ (unbox<byte> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     ^^^ (unbox<uint16> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     ^^^ (unbox<uint32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     ^^^ (unbox<uint64> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) ^^^ (unbox<unativeint> y))
        elif aty.Equals(typeof<bigint>)     then box ((unbox<bigint> x)     ^^^ (unbox<bigint> y))
//        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      ^^^ (unbox<float> y))
//        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    ^^^ (unbox<float32> y))
        else dyn()
    else dyn()

let op_LeftShift (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_LeftShift" aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      <<< (unbox<int32> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      <<< (unbox<int32> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      <<< (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      <<< (unbox<int32> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  <<< (unbox<int32> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       <<< (unbox<int32> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     <<< (unbox<int32> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     <<< (unbox<int32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     <<< (unbox<int32> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) <<< (unbox<int32> y))
//        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      <<< (unbox<float> y))
//        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    <<< (unbox<float32> y))
        else dyn()
    else dyn()

let op_RightShift (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
    let dyn() = invokeBinOp "op_RightShift" aty bty x y
    if aty.Equals(bty) && bty.Equals(cty) then
        if aty.Equals(typeof<sbyte>)        then box ((unbox<sbyte> x)      >>> (unbox<int32> y))
        elif aty.Equals(typeof<int16>)      then box ((unbox<int16> x)      >>> (unbox<int32> y))
        elif aty.Equals(typeof<int32>)      then box ((unbox<int32> x)      >>> (unbox<int32> y))
        elif aty.Equals(typeof<int64>)      then box ((unbox<int64> x)      >>> (unbox<int32> y))
        elif aty.Equals(typeof<nativeint>)  then box ((unbox<nativeint> x)  >>> (unbox<int32> y))
        elif aty.Equals(typeof<byte>)       then box ((unbox<byte> x)       >>> (unbox<int32> y))
        elif aty.Equals(typeof<uint16>)     then box ((unbox<uint16> x)     >>> (unbox<int32> y))
        elif aty.Equals(typeof<uint32>)     then box ((unbox<uint32> x)     >>> (unbox<int32> y))
        elif aty.Equals(typeof<uint64>)     then box ((unbox<uint64> x)     >>> (unbox<int32> y))
        elif aty.Equals(typeof<unativeint>) then box ((unbox<unativeint> x) >>> (unbox<int32> y))
//        elif aty.Equals(typeof<float>)      then box ((unbox<float> x)      >>> (unbox<float> y))
//        elif aty.Equals(typeof<float32>)    then box ((unbox<float32> x)    >>> (unbox<float32> y))
        else dyn()
    else dyn()

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

let op_UnaryNegation (aty:Type) (bty:Type) (x:obj) : obj = 
    let dyn() = invokeUnaryOp "op_UnaryNegation" aty x
    if aty.Equals(bty) then
        if aty.Equals(typeof<int32>)        then box (-(unbox<int32> x))
        elif aty.Equals(typeof<float>)      then box (-(unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box (-(unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box (-(unbox<int64> x))
        elif aty.Equals(typeof<int16>)      then box (-(unbox<int16> x))
        elif aty.Equals(typeof<nativeint>)  then box (-(unbox<nativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box (-(unbox<sbyte> x))
        elif aty.Equals(typeof<decimal>)    then box (-(unbox<decimal> x))
        elif aty.Equals(typeof<bigint>)     then box (-(unbox<bigint> x))
        else dyn()
    else dyn()

let op_UnaryPlus (aty:Type) (bty:Type) (x:obj) : obj = 
    let dyn() = invokeUnaryOp "op_UnaryPlus" aty x
    if aty.Equals(bty) then //N.B. a no-op for primitives
        if aty.Equals(typeof<int32>)        then box ((~+)(unbox<int32> x))
        elif aty.Equals(typeof<float>)      then box ((~+)(unbox<float> x))
        elif aty.Equals(typeof<float32>)    then box ((~+)(unbox<float32> x))
        elif aty.Equals(typeof<int64>)      then box ((~+)(unbox<int64> x))
        elif aty.Equals(typeof<uint64>)     then box ((~+)(unbox<uint64> x))
        elif aty.Equals(typeof<uint32>)     then box ((~+)(unbox<uint32> x))
        elif aty.Equals(typeof<uint16>)     then box ((~+)(unbox<uint16> x))
        elif aty.Equals(typeof<nativeint>)  then box ((~+)(unbox<nativeint> x))
        elif aty.Equals(typeof<unativeint>) then box ((~+)(unbox<unativeint> x))
        elif aty.Equals(typeof<sbyte>)      then box ((~+)(unbox<sbyte> x))
        elif aty.Equals(typeof<byte>)       then box ((~+)(unbox<byte> x))
        elif aty.Equals(typeof<decimal>)    then box ((~+)(unbox<decimal> x))
        elif aty.Equals(typeof<bigint>)     then box ((~+)(unbox<bigint> x))
        else dyn()
    else dyn()

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    else invokeExplicitOp aty bty x

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
    let op_Addition (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
        let dyn() = invokeBinOp "op_Addition" aty bty x y                        
        if aty.Equals(bty) && bty.Equals(cty) then
            if aty.Equals(typeof<sbyte>)        then box (Checked.(+) (unbox<sbyte> x) (unbox<sbyte> y))
            elif aty.Equals(typeof<int16>)      then box (Checked.(+) (unbox<int16> x) (unbox<int16> y))
            elif aty.Equals(typeof<int32>)      then box (Checked.(+) (unbox<int32> x) (unbox<int32> y))
            elif aty.Equals(typeof<int64>)      then box (Checked.(+) (unbox<int64> x) (unbox<int64> y))
            elif aty.Equals(typeof<nativeint>)  then box (Checked.(+) (unbox<nativeint> x) (unbox<nativeint> y))
            elif aty.Equals(typeof<byte>)       then box (Checked.(+) (unbox<byte> x) (unbox<byte> y))
            elif aty.Equals(typeof<uint16>)     then box (Checked.(+) (unbox<uint16> x) (unbox<uint16> y))
            elif aty.Equals(typeof<uint32>)     then box (Checked.(+) (unbox<uint32> x) (unbox<uint32> y))
            elif aty.Equals(typeof<uint64>)     then box (Checked.(+) (unbox<uint64> x) (unbox<uint64> y))
            elif aty.Equals(typeof<unativeint>) then box (Checked.(+) (unbox<unativeint> x) (unbox<unativeint> y))
            elif aty.Equals(typeof<float>)      then box (Checked.(+) (unbox<float> x) (unbox<float> y))
            elif aty.Equals(typeof<float32>)    then box (Checked.(+) (unbox<float32> x) (unbox<float32> y))
            elif aty.Equals(typeof<string>)     then box (Checked.(+) (unbox<string> x) (unbox<string> y))
            elif aty.Equals(typeof<decimal>)    then box (Checked.(+) (unbox<decimal> x) (unbox<decimal> y))
            elif aty.Equals(typeof<bigint>)     then box (Checked.(+) (unbox<bigint> x) (unbox<bigint> y))
            else dyn()
        else dyn()

    let op_Subtraction (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
        let dyn() = invokeBinOp "op_Subtraction" aty bty x y                        
        if aty.Equals(bty) && bty.Equals(cty) then
            if aty.Equals(typeof<sbyte>)        then box (Checked.(-) (unbox<sbyte> x) (unbox<sbyte> y))
            elif aty.Equals(typeof<int16>)      then box (Checked.(-) (unbox<int16> x) (unbox<int16> y))
            elif aty.Equals(typeof<int32>)      then box (Checked.(-) (unbox<int32> x) (unbox<int32> y))
            elif aty.Equals(typeof<int64>)      then box (Checked.(-) (unbox<int64> x) (unbox<int64> y))
            elif aty.Equals(typeof<nativeint>)  then box (Checked.(-) (unbox<nativeint> x) (unbox<nativeint> y))
            elif aty.Equals(typeof<byte>)       then box (Checked.(-) (unbox<byte> x) (unbox<byte> y))
            elif aty.Equals(typeof<uint16>)     then box (Checked.(-) (unbox<uint16> x) (unbox<uint16> y))
            elif aty.Equals(typeof<uint32>)     then box (Checked.(-) (unbox<uint32> x) (unbox<uint32> y))
            elif aty.Equals(typeof<uint64>)     then box (Checked.(-) (unbox<uint64> x) (unbox<uint64> y))
            elif aty.Equals(typeof<unativeint>) then box (Checked.(-) (unbox<unativeint> x) (unbox<unativeint> y))
            elif aty.Equals(typeof<float>)      then box (Checked.(-) (unbox<float> x) (unbox<float> y))
            elif aty.Equals(typeof<float32>)    then box (Checked.(-) (unbox<float32> x) (unbox<float32> y))
            elif aty.Equals(typeof<decimal>)    then box (Checked.(-) (unbox<decimal> x) (unbox<decimal> y))
            elif aty.Equals(typeof<bigint>)     then box (Checked.(-) (unbox<bigint> x) (unbox<bigint> y))
            else dyn()
        else dyn()

    let op_Multiply (aty:Type) (bty:Type) (cty:Type) (x:obj) (y:obj) : obj = 
        let dyn() = invokeBinOp "op_Multiply" aty bty x y
        if aty.Equals(bty) && bty.Equals(cty) then
            if aty.Equals(typeof<sbyte>)        then box (Checked.(*) (unbox<sbyte> x) (unbox<sbyte> y))
            elif aty.Equals(typeof<int16>)      then box (Checked.(*) (unbox<int16> x) (unbox<int16> y))
            elif aty.Equals(typeof<int32>)      then box (Checked.(*) (unbox<int32> x) (unbox<int32> y))
            elif aty.Equals(typeof<int64>)      then box (Checked.(*) (unbox<int64> x) (unbox<int64> y))
            elif aty.Equals(typeof<nativeint>)  then box (Checked.(*) (unbox<nativeint> x) (unbox<nativeint> y))
            elif aty.Equals(typeof<byte>)       then box (Checked.(*) (unbox<byte> x) (unbox<byte> y))
            elif aty.Equals(typeof<uint16>)     then box (Checked.(*) (unbox<uint16> x) (unbox<uint16> y))
            elif aty.Equals(typeof<uint32>)     then box (Checked.(*) (unbox<uint32> x) (unbox<uint32> y))
            elif aty.Equals(typeof<uint64>)     then box (Checked.(*) (unbox<uint64> x) (unbox<uint64> y))
            elif aty.Equals(typeof<unativeint>) then box (Checked.(*) (unbox<unativeint> x) (unbox<unativeint> y))
            elif aty.Equals(typeof<float>)      then box (Checked.(*) (unbox<float> x) (unbox<float> y))
            elif aty.Equals(typeof<float32>)    then box (Checked.(*) (unbox<float32> x) (unbox<float32> y))
            elif aty.Equals(typeof<decimal>)    then box (Checked.(*) (unbox<decimal> x) (unbox<decimal> y))
            elif aty.Equals(typeof<bigint>)     then box (Checked.(*) (unbox<bigint> x) (unbox<bigint> y))
            else dyn()
        else dyn()

    let binOpLookup : System.Collections.Generic.IDictionary<string, (Type->Type->Type->obj->obj->obj)> = 
        dict
            [("op_Addition", op_Addition)
             ("op_Subtraction", op_Subtraction)
             ("op_Multiply", op_Multiply)]

    let op_UnaryNegation (aty:Type) (bty:Type) (x:obj) : obj = 
        let dyn() = invokeUnaryOp "op_UnaryNegation" aty x
        if aty.Equals(bty) then
            if aty.Equals(typeof<int32>)        then box (Checked.(~-) (unbox<int32> x))
            elif aty.Equals(typeof<float>)      then box (Checked.(~-) (unbox<float> x))
            elif aty.Equals(typeof<float32>)    then box (Checked.(~-) (unbox<float32> x))
            elif aty.Equals(typeof<int64>)      then box (Checked.(~-) (unbox<int64> x))
            elif aty.Equals(typeof<int16>)      then box (Checked.(~-) (unbox<int16> x))
            elif aty.Equals(typeof<nativeint>)  then box (Checked.(~-) (unbox<nativeint> x))
            elif aty.Equals(typeof<sbyte>)      then box (Checked.(~-) (unbox<sbyte> x))
            elif aty.Equals(typeof<decimal>)    then box (Checked.(~-) (unbox<decimal> x))
            elif aty.Equals(typeof<bigint>)     then box (Checked.(~-) (unbox<bigint> x))
            else dyn()
        else dyn()

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
        else invokeExplicitOp aty bty x

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
        else invokeExplicitOp aty bty x

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
        else invokeExplicitOp aty bty x

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
        else invokeExplicitOp aty bty x

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
        else invokeExplicitOp aty bty x

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
        else invokeExplicitOp aty bty x

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
        else invokeExplicitOp aty bty x

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
        else invokeExplicitOp aty bty x
    
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
        else invokeExplicitOp aty bty x

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
        else invokeExplicitOp aty bty x

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
        else invokeExplicitOp aty bty x

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