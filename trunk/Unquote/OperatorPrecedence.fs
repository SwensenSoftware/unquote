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
| None
| Left
| Right

type OperatorPrecedence = 
    ///Precedence
   {Precedence:int; 
    ///Associativity
    Associativity: assoc}
with
    ///The precedence context to apply to expressions on the left hand side of an expresion with this precedence (depends on Associativity)
    member this.LeftContext =
        match this.Associativity with
        | Left -> this.Precedence - 1
        |_ -> this.Precedence
    ///The precedence context to apply to expressions on the right hand side of an expresion with this precedence (depends on Associativity)
    member this.RightContext =
        match this.Associativity with
        | Right -> this.Precedence - 1
        |_ -> this.Precedence
    ///Construct a new OpPrec
    static member mk(p,a) =
        {Precedence=p; Associativity=a}

let private mk = OperatorPrecedence.mk

//"Op" suffix indicates a legitimate customizable op
let As = mk(1,Right)
let When = mk(2,Right)
let Pipe = mk(3,Left)
let Semicolon = mk(4,Right) //Sequential
let Let = mk(5,None)
let Function,Fun,Match,Try,While,For = let p = mk(6,None) in p,p,p,p,p,p //While and For are not in spec, but seems at home here
let If = mk(7,None)
let RightArrow = mk(8,Right)
let RefAssign = mk(9,Right)
let Comma = mk(10,None)
let Or = mk(11,Left) //note "or" is deprecated form of "||"
let And = mk(12,Left) //note "&" is deprecated form of "&&"
let LessThanOp,GreaterThanOp,EqualsOp,PipeOp,AndOp = let p = mk(13,Left) in p,p,p,p,p
let BitwiseAnd,BitwiseOr,ExclusiveOr,LogicalNot,LeftShift,RightShift = let p = mk(14,Left) in p,p,p,p,p,p
let ConcatenateOp = mk(15,Right) //OCaml string concat
let Cons = mk(16,Right)
let AppendOp = mk(17,Left) //not sure, empirical
let DynamicCast,TypeTest = let p = mk(17,None) in p,p
let MinusBinaryOp,PlusBinaryOp = let p = mk(18,Left) in p,p
let MultiplyOp,DivideOp,ModOp = let p = mk(19,Left) in p,p,p
let ExponentiationOp = mk(20,Right)
let Application = mk(21,Left)
let PatternMatch = mk(22,Right)
let PrefixOps = mk(23,Left)
let Dot = mk(24,Left)
let MethodCall = mk(25,Left)
let TypeArguments = mk(26,Left)
