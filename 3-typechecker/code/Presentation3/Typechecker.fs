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

    let rec typeValueExp = function
        | Constant l -> typeConstant l
        | Property p ->
            match ctx.getPropType p with
                | Some t -> t
                | None -> failwith <| sprintf "undefined property %A" p
        | Func(fname, argExps) ->
            match ctx.getFuncType fname with
                | None -> failwith <| sprintf "undefined fucntion %A" fname
                | Some ftype -> checkFuncCall (fname, argExps) ftype
    and checkFuncCall (fname, argExps) (freturnType, fargTypes) =
        let argTypes = argExps |> List.map typeValueExp
        checkArgs argTypes fargTypes fname
        freturnType
    and checkArgs argTypes fargTypes fname =
        List.iteri2 (fun idx argType fargType ->
            if(not(argType.Equals(fargType)))then
                failwith <| sprintf "Argument of func %A at position %A has type %A, but %A was expected" fname idx argType fargType
        ) argTypes fargTypes

    let rec typeComplexBoolExp = function
        | Comparison(left, op, right) ->
            let leftT = typeValueExp left
            let rightT = typeValueExp right
            if(not(leftT.Equals(rightT)))then
                failwith <| sprintf "Type of relation operation arguments are not the same: %A and %A" leftT rightT
            // result of relation operation is always of type bool
            typeof<bool>
        | Not e -> typeComplexBoolExp e
        | And es -> es |> List.map typeComplexBoolExp |> ignore; typeof<bool>
        | Or es -> es |> List.map typeComplexBoolExp |> ignore; typeof<bool>

    let typeRule = function
        | Eval(c) -> typeComplexBoolExp c
        | Exec(ifs, els) ->
            ifs |> List.iter (fun (c, a) ->
                typeComplexBoolExp c |> ignore;
                checkFuncCall a |> ignore;
            )
            match els with
                | Some a -> checkFuncCall a |> ignore; typeof<bool>
                | None -> typeof<bool>


    typeRule exp

let buildTypeCtx<'model> () =
    {
        getPropType = fun p ->
            match typeof<'model>.GetProperty(p) with
            | null -> None
            | pi -> pi.GetGetMethod().ReturnType |> Some
        getFuncType = fun f ->
            match typeof<'model>.GetMethod(f) with
            | null -> None
            | mi ->
                let argTypes = mi.GetParameters() |> Array.map (fun p -> p.ParameterType) |> List.ofArray
                Some(mi.ReturnType, argTypes)
    }

let typecheck<'model> exp =
    let typectx = buildTypeCtx<'model>()
    let t = inferType typectx exp // will throw on error
    exp
