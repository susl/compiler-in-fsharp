﻿module CompileWithIL

open System
open System.Reflection
open System.Reflection.Emit

open Ast

let compileToIL<'model> (il : ILGenerator) exp =
    let compileConstant = function
        | Int i -> il.Emit(OpCodes.Ldc_I4, i)
        | String s -> il.Emit(OpCodes.Ldstr, s)
        | Bool b -> if b then il.Emit(OpCodes.Ldc_I4_1) else il.Emit(OpCodes.Ldc_I4_0)

    let compileOp = function
        | Eq -> il.Emit(OpCodes.Ceq)
        | Lt -> il.Emit(OpCodes.Clt)
        | Gt -> il.Emit(OpCodes.Cgt)

    let rec compileValueExp = function
        | Constant l -> compileConstant l
        | Property p ->
            let prop = typeof<'model>.GetProperty(p, BindingFlags.Instance ||| BindingFlags.Public)
            let getter = prop.GetGetMethod(true)
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Callvirt, getter)
        | Func f -> compileCall f
    and compileCall(fname, argExps) = 
            il.Emit(OpCodes.Ldarg_0)
            argExps |> List.iter compileValueExp
            il.Emit(OpCodes.Callvirt, typeof<'model>.GetMethod(fname))

    let rec compileComplexBoolExp = function
        | Comparison(left, op, right) -> compileValueExp left; compileValueExp right; compileOp op
        | Not e -> compileComplexBoolExp e; il.Emit(OpCodes.Not)
        | And es ->
            il.Emit(OpCodes.Ldc_I4_1)
            es |> List.fold (fun _ e -> compileComplexBoolExp e; il.Emit(OpCodes.And)) ()
        | Or es -> 
            il.Emit(OpCodes.Ldc_I4_0)
            es |> List.fold (fun _ e -> compileComplexBoolExp e; il.Emit(OpCodes.Or)) ()

    let compileRule = function
        | Eval(c) -> compileComplexBoolExp c
        | Exec(ifs, els) -> 
            ifs |> List.iter (fun (cond, call) -> 
                compileComplexBoolExp cond
                il.Emit(OpCodes.Ldc_I4_0)
                il.Emit(OpCodes.Ceq)
                let l = il.DefineLabel()
                il.Emit(OpCodes.Brtrue, l)
                compileCall call
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Ret)
                il.MarkLabel(l)
            )
            els |> Option.iter (fun call -> 
                compileCall call
                il.Emit(OpCodes.Ldc_I4_1)
                il.Emit(OpCodes.Ret)
            )
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ret)                

    compileRule exp
    il.Emit(OpCodes.Ret)

let compileToDynamicMethod<'model> exp =
    let name = Guid.NewGuid().ToString()
    let method' = new DynamicMethod(name, typeof<bool>, [| typeof<'model> |])
    let il = method'.GetILGenerator()

    compileToIL<'model> il exp
    
    let compiledFun = method'.CreateDelegate(typeof<Func<'model, bool>>) :?> Func<'model, bool>
    let result = fun model -> compiledFun.Invoke(model)
    result
