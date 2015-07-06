module LexerTokens

type op = string
type token =
| EOF
| INT of int
| STRING of string
| BOOL of bool
| AND | OR | NOT
| OP of op
| LP | RP
| COMMA
| IDENTIFIER of string
| IF | THEN | ELSE
| CHECK

let ops: op list = ["="; "<"; ">"]
