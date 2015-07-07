module Ast

// not(Amount < 10) and HasTag('vip') = true

type ast = complexBoolExp
and complexBoolExp =
    | Comparison of valueExp * op * valueExp
    | Not of complexBoolExp
    | And of complexBoolExp list
    | Or of complexBoolExp list
and op = Eq | Lt | Gt
and valueExp =
    | Constant of constant
    | Property of identifier
    | Func of identifier * valueExp list
and constant =
    | Int of int
    | String of string
    | Bool of bool
and identifier = string

module Examples =
    // 5 < 10 and "a" = "a"
    let simpleExp =
        And [
            Comparison(
                Constant(Int 5),
                Lt,
                Constant(Int 10)
            );
            Comparison(
                Constant(String "a"),
                Eq,
                Constant(String "a")
            );
        ]
    // Amount < 10
    let checkAmount = Comparison(Property "Amount", Lt, Constant(Int 10))
    // HasTag('vip') = true
    let hasVipTag = Comparison(Func("HasTag", [Constant(String "vip")]), Eq, Constant(Bool true))
    // not(Amount < 10) and (HasTag('vip') = true)
    let complexExp = And([Not(checkAmount); hasVipTag])
