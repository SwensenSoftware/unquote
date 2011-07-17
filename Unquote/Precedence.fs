module Swensen.Unquote.OperatorPrecedence

type assoc =
| None
| Left
| Right

//"Op" suffix indicates a legitimate customizable op
let As = 1,Right
let When = 2,Right
let Pipe = 3,Left
let Semicolon = 4,Right //Sequential
let Let = 5,None
let Function,Fun,Match,Try = let p = 6,None in p,p,p,p
let If = 7,None
let RightArrow = 8,Right
let RefAssign = 9,Right
let Comma = 10,None
let Or = 11,Left //note "or" is deprecated form of "||"
let And = 12,Left //note "&" is deprecated form of "&&"
let LessThanOp,GreaterThanOp,EqualsOp,PipeOp,AndOp = let p = 13,Left in p,p,p,p,p
let BitwiseAnd,BitwiseOr,ExclusiveOr,LogicalNot,LeftShift,RightShift = let p = 14,Left in p,p,p,p,p,p
let ConcatenateOp = 15,Right //OCaml string concat
let Cons = 16,Right
let AppendOp = 17,Left //not sure, empirical
let DynamicCast,TypeTest = let p = 17,None in p,p
let MinusBinaryOp,PlusBinaryOp = let p = 18,Left in p,p
let MultiplyOp,DivideOp,ModOp = let p = 19,Left in p,p,p
let ExponentiationOp = 20,Right
let Application = 21,Left
let PatternMatch = 22,Right
let PrefixOps = 23,Left
let Dot = 24,Left
let MethodCall = 25,Left
let TypeArguments = 26,Left
