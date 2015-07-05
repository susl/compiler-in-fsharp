module CompileWithLinq

open Ast
open System.Linq.Expressions

let compile<'model> exp =
    let param = Expression.Parameter(typeof<'model>, "model")

    let compileConstant = function
        | Int i -> Expression.Constant(i, typeof<int>)
        | String s -> Expression.Constant(s, typeof<string>)
        | Bool b -> Expression.Constant(b, typeof<bool>)

    let rec compileValueExp: valueExp -> Expression = function
        | Constant l -> upcast compileConstant l
        | Property p -> upcast Expression.PropertyOrField(param :> Expression, p)
        | Func f -> upcast callFunction f
    and callFunction(fname, argExps) =
        let args = argExps |> Array.ofList |> Array.map compileValueExp
        Expression.Call(param, fname, null, args)

    let compileOp = function
        | Eq -> fun l r -> Expression.Equal(l, r)
        | Lt -> fun l r -> Expression.LessThan(l, r)
        | Gt -> fun l r -> Expression.GreaterThan(l, r)

    let rec compileComplexBoolExp: complexBoolExp -> Expression = function
        | Comparison(left, op, right) ->
            let left' = compileValueExp left
            let right' = compileValueExp right
            let op' = compileOp op
            upcast op' left' right'
        | Not e -> e |> compileComplexBoolExp |> Expression.Not :> Expression
        | And es -> es |> List.map compileComplexBoolExp |> List.reduce (fun l r -> upcast Expression.AndAlso(l, r))
        | Or es -> es |> List.map compileComplexBoolExp |> List.reduce (fun l r -> upcast Expression.OrElse(l, r))

    let compileRule = function
        | Eval(c) -> compileComplexBoolExp c
        | Exec(ifs, els) ->
            let label = Expression.Label(typeof<bool>)
            let ret (b: bool) = Expression.Return(label, Expression.Constant(b), typeof<bool>) :> Expression

            let compileSingleIfCase(cond, call) =
                let cond' = compileComplexBoolExp cond
                let call' = callFunction call :> Expression
                let body = Expression.Block([| call'; ret true |])
                Expression.IfThen(cond', body) :> Expression

            let ifs' = ifs |> List.map compileSingleIfCase
            let els' =
                match els with
                    | None -> Expression.Empty() :> Expression
                    | Some e -> Expression.Block([| callFunction e :> Expression; ret true |]) :> Expression

            let body = ifs' @ [els'; Expression.Label(label, ret false)] |> Array.ofList
            upcast Expression.Block(body)

    let body = compileRule exp
    let lambda = Expression.Lambda<System.Func<'model, bool>>(body, [| param |])
    lambda

let compileToDynamicMethod<'model> exp =
    let lambda = compile<'model> exp
    let compiled = lambda.Compile()
    fun model -> compiled.Invoke(model)

open System
open System.Reflection
open System.Reflection.Emit

let compileToDll<'model> exp methodName assemblyName = 
    let appDomain = AppDomain.CurrentDomain
    let fileName = assemblyName + ".dll"

    let assemblyBuilder = appDomain.DefineDynamicAssembly(AssemblyName(assemblyName), AssemblyBuilderAccess.Save)

    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName, fileName)
    let typeBuilder = moduleBuilder.DefineType("Rules", TypeAttributes.Public)

    let methodBuilder = 
        typeBuilder.DefineMethod(
            methodName, 
            MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.HideBySig,
            typeof<bool>, [| typeof<'model> |])

    let lambda = compile<'model> exp
    lambda.CompileToMethod(methodBuilder)

    typeBuilder.CreateType() |> ignore
    assemblyBuilder.Save(fileName)
