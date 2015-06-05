module public RuleCompiler

open System
open Main

let public Compile<'TModel>(rule: string): System.Func<'TModel, bool> =
    let compiled = compile<'TModel> rule
    new System.Func<'TModel, bool>(compiled)
