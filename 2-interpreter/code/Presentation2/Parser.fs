﻿module Parser

open Ast

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
