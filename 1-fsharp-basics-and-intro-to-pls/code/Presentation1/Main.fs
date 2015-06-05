module Main

open System

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


// ============= Interpreter

type context =
    { getPropValue: identifier -> obj; getFunc: identifier -> (obj list -> obj) }

//let ctx: evalContext = 
//    {
//        getPropValue = fun x -> x :> obj;
//        getFunc = fun _ -> (fun x -> x :> obj);
//    }

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

    // ideally, we want this, but we have to jump over some hoops because of .net
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

// ============= Compile To Linq Expressions

open System.Linq.Expressions

let compileToLinq<'model> exp =
    let model = Expression.Parameter(typeof<'model>, "model")

    let compileConstant = function
        | Int i -> Expression.Constant(i, typeof<int>)
        | String s -> Expression.Constant(s, typeof<string>)
        | Bool b -> Expression.Constant(b, typeof<bool>)

    let compileOp = function
        | Eq -> fun l r -> Expression.Equal(l, r)
        | Lt -> fun l r -> Expression.LessThan(l, r)
        | Gt -> fun l r -> Expression.GreaterThan(l, r)

    let rec compileValueExp = function
        | Constant l -> compileConstant l :> Expression
        | Property p -> Expression.PropertyOrField(model, p) :> Expression
        | Func(fname, argExps) ->
            let args = argExps |> List.map compileValueExp |> Array.ofList
            Expression.Call(model, fname, null, args) :> Expression

    let rec compileComplexBoolExp = function
        | Comparison(left, op, right) ->
            let left' = compileValueExp left
            let right' = compileValueExp right
            let op' = compileOp op
            (op' left' right') :> Expression
        | Not e -> e |> compileComplexBoolExp |> Expression.Not :> Expression
        | And es -> es
                    |> List.map compileComplexBoolExp
                    |> List.reduce (fun l r -> Expression.AndAlso(l, r) :> Expression)
        | Or es -> es
                    |> List.map compileComplexBoolExp
                    |> List.reduce (fun l r -> Expression.OrElse(l, r) :> Expression)

    let body = compileComplexBoolExp exp
    let compiledExp = Expression.Lambda(body, model)
    let compiledFun = compiledExp.Compile() :?> Func<'model, bool>

    let result = fun model -> compiledFun.Invoke(model)
    result

// ============= Typechecker

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
                | Some(freturnType, fargTypes) ->
                    let argTypes = argExps |> List.map typeValueExp
                    // check argument types
                    List.iteri2 (fun idx argType fargType ->
                        if(not(argType.Equals(fargType)))then
                            failwith <| sprintf "Argument of func %A at position %A has type %A, but %A was expected" fname idx argType fargType
                    ) argTypes fargTypes
                    // return function return type
                    freturnType

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

    typeComplexBoolExp exp

let typecheck typectx exp =
    let t = inferType typectx exp // will throw on error
    exp

// ============= Parser

open FParsec

// helpers
let ws = spaces
let str_ws s = pstring s .>> ws

let pidentifier = many1Chars (satisfy isAsciiLetter)

let ptrue = stringReturn "true" true
let pfalse = stringReturn "false" false
let pboolConstant = (ptrue <|> pfalse) |>> Bool
// we're cheating here, we do not parse escaped characters inside strings
let pstringConstant = skipStringCI "'" >>. manyCharsTill anyChar (skipStringCI "'") |>> String
let pintConstant = pint32 |>> Int

let pconstant = (pintConstant <|> pboolConstant <|> pstringConstant) |>> Constant
let pproperty = pidentifier |>> Property
let pfunccall, pfunccallimpl = createParserForwardedToRef ()
let pvalue = choice[ pconstant; attempt pfunccall <|> attempt pproperty ]

let pargs = between (str_ws "(") (str_ws ")") (sepBy pvalue (str_ws ","))
pfunccallimpl := tuple2 pidentifier pargs |>> Func

let poperator = ws >>. choice[stringReturn "=" Eq; stringReturn "<" Lt; stringReturn ">" Gt] .>> ws
let pcomparison = tuple3 pvalue poperator pvalue |>> Comparison

let oppl = new OperatorPrecedenceParser<complexBoolExp,unit,unit>()
let pcomplex = oppl.ExpressionParser
let psimple = (pcomparison .>> ws) <|> between (str_ws "(") (str_ws ")") pcomplex
oppl.TermParser <- psimple
oppl.AddOperator(InfixOperator("or", ws, 1, Associativity.Left, fun x y -> Or [x; y]))
oppl.AddOperator(InfixOperator("and", ws, 2, Associativity.Left, fun x y -> And [x; y]))
oppl.AddOperator(PrefixOperator("not", ws, 3, true, Not))

let parse =
    run pcomplex >> function
        | Success(result, _, _) -> result
        | Failure(_, error, _) -> failwith <| sprintf "%A" error

// ============= Compile to IL

open System.Reflection
open System.Reflection.Emit

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
        | Func(fname, argExps) ->
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

    compileComplexBoolExp exp
    il.Emit(OpCodes.Ret)

let compileToDynamicMethod<'model> exp =
    let name = Guid.NewGuid().ToString()
    let method' = new DynamicMethod(name, typeof<bool>, [| typeof<'model> |])
    let il = method'.GetILGenerator()

    compileToIL<'model> il exp
    
    let compiledFun = method'.CreateDelegate(typeof<Func<'model, bool>>) :?> Func<'model, bool>
    let result = fun model -> compiledFun.Invoke(model)
    result

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
    let il = methodBuilder.GetILGenerator()
    compileToIL<'model> il exp

    typeBuilder.CreateType() |> ignore
    assemblyBuilder.Save(fileName)

// ============= Putting everything together

let compile<'model> : string -> ('model -> bool) =
    let typectx =
        {
            getPropType = fun name ->
                match typeof<'model>.GetProperty(name) with
                    | null -> None
                    | x -> Some(x.PropertyType);
            getFuncType = fun name ->
                match typeof<'model>.GetMethod(name) with
                    | null -> None
                    | x ->
                        let argTypes = x.GetParameters() |> Array.map (fun (p: Reflection.ParameterInfo) -> p.ParameterType) |> List.ofArray
                        Some(x.ReturnType, argTypes)
        }
    parse
    >> typecheck typectx
    >> compileToLinq<'model>
    //>> compileToDynamicMethod<'model>
