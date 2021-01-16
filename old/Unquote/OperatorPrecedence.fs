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
module internal Swensen.Unquote.OperatorPrecedence

type assoc =
| Non
| Left
| Right

//note that we deliberately use referential transpency to apply "special" precedence rules. see applyParensForPrecInContext.
///Represents an operator's precedence. The lower the precedence value, the lower the binding.
type OperatorPrecedence(precedence:int, ?associativity:assoc) = 
    let associativity = match associativity with Some(a) -> a | None -> Non
    ///Precedence
    member __.Precedence = precedence
    ///Associativity
    member __.Associativity = associativity

type OP = OperatorPrecedence

//"Op" suffix indicates a legitimate customizable op
let As = OP(1,Right)
let When = OP(2,Right)
let Pipe = OP(3,Left)
let Semicolon = OP(4,Right) //Sequential
let Let = OP(5)
let Function,Fun,Match,Try,While,For = let p() = OP(6) in p(),p(),p(),p(),p(),p() //While and For are not in spec, but seems at home here
let If = OP(7)
let RightArrow = OP(8,Right)
let RefAssign = OP(9,Right)
let Comma = OP(10)
let LogicalOr = OP(11,Left) //note "or" is deprecated form of "||"
let LogicalAnd = OP(12,Left) //note "&" is deprecated form of "&&"
let StaticCast,DynamicCast,LessThanOp,GreaterThanOp,EqualsOp,PipeOp,AndOp,BangEqualsOp, DollarOp = let p() = OP(14,Left) in p(),p(),p(),p(),p(),p(),p(),p(),p()
let BitwiseAnd,BitwiseOr,ExclusiveOr,LogicalNot,LeftShift,RightShift = let p() = OP(15,Left) in p(),p(),p(),p(),p(),p()
let ConcatenateOp = OP(16,Right) //OCaml string concat
let Cons = OP(17,Right)
let AppendOp = OP(18,Left) //not sure, empirical
let TypeTest = OP(18)
let MinusBinaryOp,PlusBinaryOp = let p() = OP(19,Left) in p(),p()
let MultiplyOp,DivideOp,ModOp = let p() = OP(20,Left) in p(),p(),p()
let ExponentiationOp = OP(21,Right)
let Application = OP(22,Left)
let PatternMatch = OP(23,Right)
let PrefixOps = OP(24,Left)
let Dot = OP(25,Left)
let MethodCall = OP(26,Left)
let TypeArguments = OP(27,Left)

let applyParensForPrecInContext (contextOP:OperatorPrecedence) contextAssoc (localOP:OperatorPrecedence) s = 
    let parenthesize = sprintf "(%s)"
    if contextOP = Application && localOP = MethodCall then //special rule for e.g. "fsFunction (unitCall())"
        parenthesize s
    elif contextOP = Application && contextAssoc = Left && localOP = PrefixOps then //special rule for e.g. "(-x) y" application when - is a prefix op returning a function
        parenthesize s
    else //normal rules
        let context =
            match contextOP.Associativity, contextAssoc with
            | Left, Left | Right, Right -> contextOP.Precedence - 1
            | _ -> contextOP.Precedence
    
        if localOP.Precedence > context then s else parenthesize s