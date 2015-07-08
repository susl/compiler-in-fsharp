module RecursiveDescentParser

open Ast
open LexerTokens

module internal Inner = 
    let expect token = function
        | t :: rest when t = token -> rest 
        | t :: _ -> failwith <| sprintf "unexpected token %A, expected %A" t token
        | [] -> failwith <| sprintf "unexpected end of stream, expected %A" token

    let expectEOF tokens = 
        match expect EOF tokens with
            | [] -> ()
            | rest -> failwith "unexpected tokens after EOF, should be end of stream: %A" rest

    let parseOp = function
        | OP "=" :: rest -> Eq, rest
        | OP "<" :: rest -> Lt, rest
        | OP ">" :: rest -> Gt, rest
        | _ -> failwith "expected Equality or comparison operator"

    let rec parseValueExp = function
        | INT i :: rest -> Constant(Int i), rest
        | STRING s :: rest -> Constant(String s), rest
        | BOOL b :: rest -> Constant(Bool b), rest
        | IDENTIFIER f :: LP :: rest as tokens-> 
            let f, rest = parseFuncCall tokens
            Func f, rest
        | IDENTIFIER p :: rest -> Property p, rest
        | _ -> failwith "unexpected function argument, expected literal, property name or function call"

    and parseArgs tokens =
        let rec loop acc = function
            | RP :: _ as tokens -> (List.rev acc), tokens
            | COMMA :: rest -> 
                let arg, rest = parseValueExp rest
                loop (arg :: acc) rest
            | _ -> failwith "expected either COMMA for next arg or RP to end function call"
    
        match tokens with
        | RP :: _ -> [], tokens // no arguments given
        | _ -> 
            let arg, rest = parseValueExp tokens
            loop [arg] rest

    and parseFuncCall tokens = 
        match tokens with
        | IDENTIFIER funcName :: rest ->
            let rest = expect LP rest
            let args, rest = parseArgs rest
            let rest = expect RP rest
            (funcName, args), rest
        | _ -> failwith "expected function name"

    let rec loopBinOp optoken opfunc acc = function
        | t :: rest when t = optoken ->
            let a, rest = opfunc rest
            loopBinOp optoken opfunc (a :: acc) rest
        | rest -> (List.rev acc), rest

    let rec parseAtomExp tokens = 
        match tokens with
        | LP :: rest -> 
            let b, rest = parseBoolExp rest
            let rest = expect RP rest
            b, rest
        | _ -> 
            match parseValueExp tokens with
            | left, (OP op :: _ as rest) ->
                let op, rest = parseOp rest
                let right, rest = parseValueExp rest
                Comparison(left, op, right), rest
            | _ -> failwith "expected comparison expression"

    and parseBoolExp = function
        | NOT :: rest -> 
            let b, rest = parseBoolExp rest
            Not b, rest
        | tokens ->
            let atom, rest = parseAtomExp tokens
            // this does not handle OR and AND (and NOT) on the same level
            // something should be in parenthesis, but this is ok for rule engine
            match rest with
            | (OR as op) :: _ | (AND as op) :: _ -> 
                let l, rest = loopBinOp op parseAtomExp [atom] rest
                let ast = match op with OR -> Or l | AND -> And l | _ -> failwith "unexpected"
                ast, rest
            | _ -> atom, rest

    let parseCond tokens = parseBoolExp tokens 

    let parseEval tokens = 
        let rest = expect CHECK tokens
        let cond, rest = parseCond rest
        expectEOF rest
        Eval cond

    let parseExec tokens = 
        let rec parseFuncCalls acc tokens =
            let f, rest = parseFuncCall tokens
            let acc = f :: acc
            match rest with
            | AND :: rest -> parseFuncCalls acc rest
            | _ -> (List.rev acc, rest)

        let parseSingleMatch tokens =
            match parseCond tokens with
            | cond, THEN :: rest -> 
                let action, rest = parseFuncCalls [] rest
                ((cond, action), rest)
            | _ -> failwith "expected THEN"

        let rec loop acc = function
            | IF :: rest ->
                if acc <> [] then failwith "unexpected IF, use ELSE IF"
                let single, rest = parseSingleMatch rest
                loop (single :: acc) rest
            | ELSE :: IF :: rest ->
                if acc = [] then failwith "unexpected ELSE IF, use IF"
                let single, rest = parseSingleMatch rest
                loop (single :: acc) rest                
            | ELSE :: rest ->
                let actions, rest = parseFuncCalls [] rest
                (acc, Some actions), rest
            | rest ->
                (acc, None), rest

        let (matches, els), rest = loop [] tokens
        expectEOF rest
        Exec (List.rev matches, els)


let parse: token list -> ast = function
  | IF :: _ as tokens -> Inner.parseExec tokens
  | tokens -> Inner.parseEval tokens
