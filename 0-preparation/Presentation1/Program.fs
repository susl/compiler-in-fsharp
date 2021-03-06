﻿open Main

let evaltests (eval: ast -> bool) =
    let test expr expected =
        let actual = eval expr
        assert (expected = actual)
        printfn "Expr: %A was evaluated to %A" expr actual

    test (Eval(checkAmount)) false;
    test (Eval(hasVipTag)) true;
    test (Eval(complexExp)) true;

    test complexExec true;

let testEval () =
    let ctx =
        {
            getPropValue = function
                | "Amount" -> 42 |> box
                | x -> failwith <| sprintf "unknown property %s" x;
            getFunc = function
                | "HasTag" ->
                    fun [arg1] ->
                        let tag = arg1 :?> string
                        tag = "vip" |> box
                | "Refuse" -> 
                    fun [arg1] ->
                        let reason = arg1 :?> string
                        printfn "Refused with %s" reason;
                        () |> box
                | x -> failwith <| sprintf "unknown function %s" x
        }

    evaltests (eval ctx)

type Model(amount: int, tags: string list) =
    member x.Amount = amount
    member x.HasTag(tag) = tags |> List.exists ((=) tag)
    override x.ToString() = sprintf "Model(%A, %A)" amount tags

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

let testCompileToLinq () =
    let model = Model(42, ["vip"])

    let compileAndRun exp =
        let compiled = compileToLinq<Model> exp
        compiled model

    evaltests compileAndRun

let testCompileToIL () =
    let model = Model(42, ["vip"])

    let compileAndRun exp =
        let compiled = compileToDynamicMethod<Model> exp
        compiled model

    evaltests compileAndRun

let testCompileToDLL () =
    compileToDll<Model> (Eval(complexExp)) "Complex" "CompiledRules"
    //compileToDll<Model> checkAmount "Complex" "CompiledRules"
    //compileToDll<Model> (Not(checkAmount)) "Complex" "CompiledRules"
    //compileToDll<Model> hasVipTag "Complex" "CompiledRules"

let testTypechecker () =
    let typectx =
        {
            getPropType = function
                | "Amount" -> Some typeof<int>
                | _ -> None;
            getFuncType = function
                | "HasTag" -> Some(typeof<bool>, [typeof<string>])
                | "Refuse" -> Some(typeof<unit>, [typeof<string>])
                | _ -> None
        }

    let test exp =
        printfn "Testing: %A" exp
        assert((inferType typectx exp) = typeof<bool>)

    test (Eval(checkAmount))
    test (Eval(hasVipTag))
    test (Eval(complexExp))
    test complexExec

let testParser () =
    let test s exp =
        let parsed = parse s
        printfn "%A parsed to %A, expected: %A" s parsed exp
        assert(exp = parsed)

    test "check Amount < 10" <| Eval(checkAmount)
    test "check HasTag('vip') = true" <| Eval(hasVipTag)
    test "check not(Amount < 10) and HasTag('vip') = true" <| Eval(complexExp)
    test "if Amount < 10 then Refuse('amount')" <| Exec([(checkAmount, refuse "amount")], None)
    test "if Amount < 10 then Refuse('amount') else if HasTag('vip') = true then Refuse('vip') else Refuse('else')" <| complexExec

let testPipeline () =
    let rule = "check Amount > 10 and HasTag('vip') = true"
    let compiled = compile<Model> rule

    let test model =
        let result = compiled model
        printfn "Running \"%A\" on %A. Result: %A" rule model result

    test <| Model(42, ["vip"])
    test <| Model(42, ["non-vip"])
    test <| Model(4, ["vip"])

open System

let printfnc s =
    let old = Console.ForegroundColor
    Console.ForegroundColor <- ConsoleColor.Green
    printfn s
    Console.ForegroundColor <- old

[<EntryPoint>]
let main argv =
    printfnc "--- Testing Interpreter ---"; testEval();
    //printfnc "--- Testing CompileToLinq ---"; testCompileToLinq();
    printfnc "--- Testing Typecheker ---"; testTypechecker();
    printfnc "--- Testing Parser ---"; testParser();
    //printfnc "--- Testing CompileToIL ---"; testCompileToIL();
    //printfnc "--- Testing CompileToDLL ---"; testCompileToDLL();
    //printfnc "=== Testing Whole Pipeline Together =="; testPipeline();
    0 // return an integer exit code
