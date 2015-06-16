let evaluate<'model> rule (model : 'model) =
    let result =
        rule
        |> Parser.parse
        |> Typechecker.typecheck<'model>
        |> Interpreter.interpret model
    printfn "Evaluated \"%s\" on %A to %A" rule model result

type Model(amount: int, tags: string list) =
    member x.Amount = amount
    member x.HasTag(tag) = tags |> List.exists ((=) tag)
    member x.Refuse(reason) = printfn "Refused with reason: %s" reason
    override x.ToString() = sprintf "Model(%A, %A)" amount tags

[<EntryPoint>]
let main argv =
    Model(42, ["vip"]) |> evaluate "check not(Amount < 10) and HasTag('vip') = true"
    Model(42, ["vip"]) |> evaluate "if Amount < 10 then Refuse('amount')"
    Model(42, ["vip"]) |> evaluate
        "if Amount < 10 then
            Refuse('amount')
        else if HasTag('vip') = true then
            Refuse('vip')
        else
            Refuse('else')"

    //Model(42, ["vip"]) |> evaluate "if Amount > 10 then Refuse(10)"

    0 // return an integer exit code
