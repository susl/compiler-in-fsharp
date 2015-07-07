let evaluate rule model =
    let ast = Parser.parse rule
    let ctx = Interpreter.buildEvalCtx model
    let result = Interpreter.eval ctx ast
    printfn "Evaluated \"%s\" on %A to %A" rule model result

type Model(amount: int, tags: string list) =
    member x.Amount = amount
    member x.HasTag(tag) = tags |> List.exists ((=) tag)
    override x.ToString() = sprintf "Model(%A, %A)" amount tags

[<EntryPoint>]
let main argv =
    Model(42, ["vip"]) |> evaluate "not(Amount < 10) and HasTag('vip') = true"

    0 // return an integer exit code
