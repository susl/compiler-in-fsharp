module CompileToDLL

open System
open System.Reflection
open System.Reflection.Emit

open Ast

let compileToDll<'model> methodName assemblyName compileFunc (exp: ast) = 
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

    compileFunc methodBuilder exp

    typeBuilder.CreateType() |> ignore
    assemblyBuilder.Save(fileName)
