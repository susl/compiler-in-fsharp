﻿module Interpreter

open System
open Ast

type context =
    { getPropValue: identifier -> obj; getFunc: identifier -> (obj list -> obj) }

let eval ctx exp =
    let evalConstant = function
        | Int i -> box i
        | String s -> box s
        | Bool b -> box b

    let rec evalValueExp = function
        | Constant l -> evalConstant l
        | Property p -> ctx.getPropValue p
        | Func(fname, argExps) ->
            let args = argExps |> List.map evalValueExp
            let f = ctx.getFunc fname
            f args

    // ideally, we want this, but we have to jump over some hoops
    let evalOp' = function
        | Eq -> (=)
        | Lt -> (<)
        | Gt -> (>)

    let evalOp: op -> (obj -> obj -> bool) = function
        | Eq -> fun x y -> (x :?> IComparable) = (y :?> IComparable)
        | Lt -> fun x y -> (x :?> IComparable) < (y :?> IComparable)
        | Gt -> fun x y -> (x :?> IComparable) > (y :?> IComparable)

    let rec evalComplexBoolExp = function
        | Comparison(left, op, right) ->
            let left' = evalValueExp left
            let right' = evalValueExp right
            let op' = evalOp op
            op' left' right'
        | Not e -> e |> evalComplexBoolExp |> not
        | And es -> es |> List.map evalComplexBoolExp |> List.reduce (&&)
        | Or es -> es |> List.map evalComplexBoolExp |> List.reduce (||)

    evalComplexBoolExp exp


let dummyctx: context =
    {
        getPropValue = fun x -> x :> obj;
        getFunc = fun _ -> (fun x -> x :> obj);
    }

let buildEvalCtx<'model> (m: 'model) =
    {
        getPropValue = fun p ->
            match typeof<'model>.GetProperty(p) with
            | null -> failwith <| sprintf "unknown property %s" p
            | pi -> pi.GetGetMethod().Invoke(m, null);
        getFunc = fun f ->
            match typeof<'model>.GetMethod(f) with
            | null -> failwith <| sprintf "unknown function %s" f
            | mi -> fun args -> mi.Invoke(m, Array.ofList args)
    }
