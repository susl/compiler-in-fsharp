let evaluate<'model> rule (model : 'model) =
    rule
    |> Lexer.tokens |> RecursiveDescentParser.parse
    //|> ParserCombinators.parse
    |> Interpreter.interpret model
    |> printfn "Evaluated \"%s\" on %A to %A" rule model

type Model(amount: int, tags: string list) =
    member x.Amount = amount
    member x.HasTag(tag) = tags |> List.exists ((=) tag)
    member x.Refuse(reason) = printfn "Refused with reason: %s" reason
    override x.ToString() = sprintf "Model(%A, %A)" amount tags

[<EntryPoint>]
let main argv =
    // TODO: not parsed correctly, because of NOT/AND priority
    Model(42, ["vip"]) |> evaluate "check not(Amount < 10) and HasTag('vip') = true"
    Model(42, ["vip"]) |> evaluate "if Amount < 10 then Refuse('amount')"
    Model(50, ["vip"]) |> evaluate
        "if Amount < 10 then
            Refuse('amount')
        else if HasTag('vip') = true then
            Refuse('vip') and Refuse('vip!!')
        else
            Refuse('else')"

    let cached = evaluate<Model> "if Amount < 10 then Refuse('amount')"

    Model(42, []) |> cached
    Model(9, []) |> cached

    0 // return an integer exit code
