let evaluate<'model> rule (model : 'model) =
    rule
    |> Parser.parse
    |> Typechecker.typecheck<'model>
    //|> Interpreter.interpret model
    //|> CompileWithLinq.compileToDynamicMethod<'model>
    |> CompileWithIL.compileToDynamicMethod<'model>
    |> (fun compiled -> compiled model |> printfn "Evaluated \"%s\" on %A to %A" rule model)

let saveToDll<'model> rule = 
    rule
    |> Parser.parse
    |> Typechecker.typecheck<'model>
    |> CompileToDLL.compileToDll<'model> "Rule" "Rules" (fun methodBuilder exp ->
        CompileWithIL.compileToIL<'model> (methodBuilder.GetILGenerator()) exp
        //(CompileWithLinq.compileToLinq<'model> exp).CompileToMethod(methodBuilder)
    )
    

type Model(amount: int, tags: string list) =
    member x.Amount = amount
    member x.HasTag(tag) = tags |> List.exists ((=) tag)
    member x.Refuse(reason) = printfn "Refused with reason: %s" reason
    override x.ToString() = sprintf "Model(%A, %A)" amount tags

[<EntryPoint>]
let main argv =
    Model(42, ["vip"]) |> evaluate "check not(Amount < 10) and HasTag('vip') = true"
    Model(42, ["vip"]) |> evaluate "if Amount < 10 then Refuse('amount')"
    Model(50, ["vip"]) |> evaluate
        "if Amount < 10 then
            Refuse('amount')
        else if HasTag('vip') = true then
            Refuse('vip')
        else
            Refuse('else')"

    let cached = evaluate<Model> "if Amount < 10 then Refuse('amount')"

    Model(42, []) |> cached
    Model(9, []) |> cached

    saveToDll<Model> "if Amount > 10 and HasTag('vip') = true then Refuse('test')"

    0 // return an integer exit code
