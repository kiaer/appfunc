// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | LPAR
  | RPAR
  | COLON
  | COMMA
  | PRINT
  | ASG
  | SKIP
  | SEMI
  | WHILE
  | IF
  | FI
  | REC
  | CALL
  | PROC
  | DO
  | OD
  | CONTOF
  | LET
  | IN
  | END
  | THEN
  | ELSE
  | RETURN
  | TRUE
  | FALSE
  | NAME of (string)
  | STRING of (string)
  | BOOL of (bool)
  | INT of (int)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_COLON
    | TOKEN_COMMA
    | TOKEN_PRINT
    | TOKEN_ASG
    | TOKEN_SKIP
    | TOKEN_SEMI
    | TOKEN_WHILE
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_REC
    | TOKEN_CALL
    | TOKEN_PROC
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_CONTOF
    | TOKEN_LET
    | TOKEN_IN
    | TOKEN_END
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_RETURN
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_NAME
    | TOKEN_STRING
    | TOKEN_BOOL
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM__startExp
    | NONTERM__startExpList
    | NONTERM__startDecList
    | NONTERM__startStm
    | NONTERM__startStmList
    | NONTERM__startDec
    | NONTERM_Main
    | NONTERM_Stm
    | NONTERM_StmList
    | NONTERM_Dec
    | NONTERM_DecList
    | NONTERM_NAMEList
    | NONTERM_Exp
    | NONTERM_ExpList
/// This function maps integers indexes to symbolic token ids
val tagOfToken: token -> int

/// This function maps integers indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Main : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Stm) 
val Exp : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Exp) 
val ExpList : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Exp list) 
val DecList : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Dec list) 
val Stm : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Stm) 
val StmList : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Stm list) 
val Dec : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Dec) 
