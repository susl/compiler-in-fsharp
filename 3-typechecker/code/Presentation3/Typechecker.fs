module Typechecker

open Ast
open System

type typecontext =
    { getPropType: identifier -> Type option; getFuncType: identifier -> (Type * Type list) option }

let inferType ctx exp =

    let typeConstant = function
        | Int i -> typeof<int>
        | String s -> typeof<string>
        | Bool b -> typeof<bool>

    ...

    typeComplexBoolExp exp

let typecheck typectx exp =
    let t = inferType typectx exp // will throw on error
    exp
