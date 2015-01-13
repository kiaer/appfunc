// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 2 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
 
open AST

# 10 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
// This type is the type of tokens accepted by the parser
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
  | DO
  | OD
  | CONTOF
  | LET
  | IN
  | END
  | TRUE
  | FALSE
  | NAME of (string)
  | STRING of (string)
  | BOOL of (bool)
  | INT of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
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
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_CONTOF
    | TOKEN_LET
    | TOKEN_IN
    | TOKEN_END
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_NAME
    | TOKEN_STRING
    | TOKEN_BOOL
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
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
    | NONTERM_Exp
    | NONTERM_ExpList

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | LPAR  -> 1 
  | RPAR  -> 2 
  | COLON  -> 3 
  | COMMA  -> 4 
  | PRINT  -> 5 
  | ASG  -> 6 
  | SKIP  -> 7 
  | SEMI  -> 8 
  | WHILE  -> 9 
  | DO  -> 10 
  | OD  -> 11 
  | CONTOF  -> 12 
  | LET  -> 13 
  | IN  -> 14 
  | END  -> 15 
  | TRUE  -> 16 
  | FALSE  -> 17 
  | NAME _ -> 18 
  | STRING _ -> 19 
  | BOOL _ -> 20 
  | INT _ -> 21 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_LPAR 
  | 2 -> TOKEN_RPAR 
  | 3 -> TOKEN_COLON 
  | 4 -> TOKEN_COMMA 
  | 5 -> TOKEN_PRINT 
  | 6 -> TOKEN_ASG 
  | 7 -> TOKEN_SKIP 
  | 8 -> TOKEN_SEMI 
  | 9 -> TOKEN_WHILE 
  | 10 -> TOKEN_DO 
  | 11 -> TOKEN_OD 
  | 12 -> TOKEN_CONTOF 
  | 13 -> TOKEN_LET 
  | 14 -> TOKEN_IN 
  | 15 -> TOKEN_END 
  | 16 -> TOKEN_TRUE 
  | 17 -> TOKEN_FALSE 
  | 18 -> TOKEN_NAME 
  | 19 -> TOKEN_STRING 
  | 20 -> TOKEN_BOOL 
  | 21 -> TOKEN_INT 
  | 24 -> TOKEN_end_of_input
  | 22 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM__startExp 
    | 2 -> NONTERM__startExpList 
    | 3 -> NONTERM__startDecList 
    | 4 -> NONTERM__startStm 
    | 5 -> NONTERM__startStmList 
    | 6 -> NONTERM__startDec 
    | 7 -> NONTERM_Main 
    | 8 -> NONTERM_Stm 
    | 9 -> NONTERM_Stm 
    | 10 -> NONTERM_Stm 
    | 11 -> NONTERM_Stm 
    | 12 -> NONTERM_StmList 
    | 13 -> NONTERM_StmList 
    | 14 -> NONTERM_Dec 
    | 15 -> NONTERM_DecList 
    | 16 -> NONTERM_DecList 
    | 17 -> NONTERM_DecList 
    | 18 -> NONTERM_Exp 
    | 19 -> NONTERM_Exp 
    | 20 -> NONTERM_Exp 
    | 21 -> NONTERM_Exp 
    | 22 -> NONTERM_Exp 
    | 23 -> NONTERM_Exp 
    | 24 -> NONTERM_Exp 
    | 25 -> NONTERM_ExpList 
    | 26 -> NONTERM_ExpList 
    | 27 -> NONTERM_ExpList 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 24 
let _fsyacc_tagOfErrorTerminal = 22

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | COLON  -> "COLON" 
  | COMMA  -> "COMMA" 
  | PRINT  -> "PRINT" 
  | ASG  -> "ASG" 
  | SKIP  -> "SKIP" 
  | SEMI  -> "SEMI" 
  | WHILE  -> "WHILE" 
  | DO  -> "DO" 
  | OD  -> "OD" 
  | CONTOF  -> "CONTOF" 
  | LET  -> "LET" 
  | IN  -> "IN" 
  | END  -> "END" 
  | TRUE  -> "TRUE" 
  | FALSE  -> "FALSE" 
  | NAME _ -> "NAME" 
  | STRING _ -> "STRING" 
  | BOOL _ -> "BOOL" 
  | INT _ -> "INT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | COLON  -> (null : System.Object) 
  | COMMA  -> (null : System.Object) 
  | PRINT  -> (null : System.Object) 
  | ASG  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | SEMI  -> (null : System.Object) 
  | WHILE  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | OD  -> (null : System.Object) 
  | CONTOF  -> (null : System.Object) 
  | LET  -> (null : System.Object) 
  | IN  -> (null : System.Object) 
  | END  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | NAME _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | STRING _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | BOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 0us; 65535us; 1us; 65535us; 0us; 1us; 6us; 65535us; 0us; 14us; 8us; 9us; 10us; 31us; 21us; 31us; 26us; 31us; 32us; 31us; 4us; 65535us; 10us; 11us; 21us; 22us; 26us; 27us; 32us; 33us; 4us; 65535us; 6us; 37us; 12us; 13us; 24us; 37us; 38us; 37us; 3us; 65535us; 6us; 7us; 24us; 25us; 38us; 39us; 10us; 65535us; 2us; 3us; 4us; 52us; 17us; 18us; 19us; 20us; 29us; 30us; 35us; 36us; 44us; 45us; 46us; 47us; 49us; 52us; 53us; 52us; 3us; 65535us; 4us; 5us; 49us; 50us; 53us; 54us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 2us; 3us; 4us; 5us; 6us; 7us; 9us; 16us; 21us; 26us; 30us; 41us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 1us; 2us; 1us; 2us; 1us; 3us; 1us; 3us; 1us; 4us; 1us; 4us; 1us; 5us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 10us; 1us; 10us; 1us; 10us; 1us; 11us; 1us; 11us; 2us; 12us; 13us; 1us; 13us; 1us; 13us; 1us; 14us; 1us; 14us; 1us; 14us; 2us; 16us; 17us; 1us; 17us; 1us; 17us; 2us; 18us; 24us; 1us; 19us; 1us; 20us; 1us; 21us; 1us; 22us; 1us; 22us; 1us; 23us; 1us; 23us; 1us; 23us; 1us; 24us; 1us; 24us; 1us; 24us; 2us; 26us; 27us; 1us; 27us; 1us; 27us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 16us; 18us; 20us; 22us; 24us; 26us; 28us; 30us; 32us; 34us; 36us; 38us; 40us; 42us; 44us; 46us; 48us; 50us; 52us; 54us; 56us; 58us; 60us; 62us; 65us; 67us; 69us; 71us; 73us; 75us; 78us; 80us; 82us; 85us; 87us; 89us; 91us; 93us; 95us; 97us; 99us; 101us; 103us; 105us; 107us; 110us; 112us; |]
let _fsyacc_action_rows = 55
let _fsyacc_actionTableElements = [|4us; 32768us; 5us; 29us; 9us; 19us; 13us; 24us; 18us; 16us; 0us; 49152us; 6us; 32768us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 0us; 49152us; 6us; 16409us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 0us; 49152us; 1us; 16399us; 18us; 34us; 0us; 49152us; 4us; 32768us; 5us; 29us; 9us; 19us; 13us; 24us; 18us; 16us; 0us; 49152us; 4us; 32768us; 5us; 29us; 9us; 19us; 13us; 24us; 18us; 16us; 0us; 49152us; 1us; 32768us; 18us; 34us; 0us; 49152us; 1us; 32768us; 0us; 15us; 0us; 16391us; 1us; 32768us; 6us; 17us; 6us; 32768us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 0us; 16392us; 6us; 32768us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 1us; 32768us; 10us; 21us; 4us; 32768us; 5us; 29us; 9us; 19us; 13us; 24us; 18us; 16us; 1us; 32768us; 11us; 23us; 0us; 16393us; 1us; 16399us; 18us; 34us; 1us; 32768us; 14us; 26us; 4us; 32768us; 5us; 29us; 9us; 19us; 13us; 24us; 18us; 16us; 1us; 32768us; 15us; 28us; 0us; 16394us; 6us; 32768us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 0us; 16395us; 1us; 16396us; 8us; 32us; 4us; 32768us; 5us; 29us; 9us; 19us; 13us; 24us; 18us; 16us; 0us; 16397us; 1us; 32768us; 3us; 35us; 6us; 32768us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 0us; 16398us; 1us; 16400us; 8us; 38us; 1us; 16399us; 18us; 34us; 0us; 16401us; 1us; 16402us; 1us; 49us; 0us; 16403us; 0us; 16404us; 0us; 16405us; 6us; 32768us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 0us; 16406us; 6us; 32768us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 1us; 32768us; 2us; 48us; 0us; 16407us; 6us; 16409us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 1us; 32768us; 2us; 51us; 0us; 16408us; 1us; 16410us; 4us; 53us; 6us; 16409us; 1us; 46us; 12us; 44us; 18us; 40us; 19us; 43us; 20us; 42us; 21us; 41us; 0us; 16411us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 13us; 14us; 21us; 22us; 24us; 25us; 30us; 31us; 36us; 37us; 39us; 40us; 42us; 43us; 45us; 52us; 53us; 60us; 62us; 67us; 69us; 70us; 72us; 74us; 79us; 81us; 82us; 89us; 90us; 92us; 97us; 98us; 100us; 107us; 108us; 110us; 112us; 113us; 115us; 116us; 117us; 118us; 125us; 126us; 133us; 135us; 136us; 143us; 145us; 146us; 148us; 155us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 1us; 1us; 1us; 1us; 1us; 2us; 3us; 5us; 5us; 2us; 1us; 3us; 3us; 0us; 1us; 3us; 1us; 1us; 1us; 1us; 2us; 3us; 4us; 0us; 1us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 4us; 5us; 6us; 7us; 8us; 8us; 8us; 8us; 9us; 9us; 10us; 11us; 11us; 11us; 12us; 12us; 12us; 12us; 12us; 12us; 12us; 13us; 13us; 13us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 49152us; 65535us; 16391us; 65535us; 65535us; 16392us; 65535us; 65535us; 65535us; 65535us; 16393us; 65535us; 65535us; 65535us; 65535us; 16394us; 65535us; 16395us; 65535us; 65535us; 16397us; 65535us; 65535us; 16398us; 65535us; 65535us; 16401us; 65535us; 16403us; 16404us; 16405us; 65535us; 16406us; 65535us; 65535us; 16407us; 65535us; 65535us; 16408us; 65535us; 65535us; 16411us; |]
let _fsyacc_reductions ()  =    [| 
# 231 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 240 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startExp));
# 249 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Exp list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startExpList));
# 258 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Dec list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startDecList));
# 267 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startStm));
# 276 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startStmList));
# 285 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Dec)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startDec));
# 294 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                               _1 
                   )
# 27 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Stm));
# 305 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                             Asg(Var _1,_3) 
                   )
# 30 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Stm));
# 317 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Stm list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                       While(_2,Seq _4) 
                   )
# 31 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Stm));
# 329 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Dec list)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Stm list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                Block(_2,Seq _4) 
                   )
# 32 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Stm));
# 341 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                PrintLn _2 
                   )
# 33 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Stm));
# 352 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                               [_1] 
                   )
# 36 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Stm list));
# 363 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Stm)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Stm list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                               _1 :: _3 
                   )
# 37 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Stm list));
# 375 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                 VarDec(_1,_3) 
                   )
# 40 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Dec));
# 387 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                               [] 
                   )
# 43 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Dec list));
# 397 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Dec)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                               [_1] 
                   )
# 44 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Dec list));
# 408 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Dec)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Dec list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 45 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                               _1 :: _3 
                   )
# 45 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Dec list));
# 420 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                Var _1  
                   )
# 48 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp));
# 431 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                Int _1 
                   )
# 49 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp));
# 442 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 50 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                Bool _1 
                   )
# 50 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp));
# 453 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                String _1
                   )
# 51 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp));
# 464 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                ContOf _2 
                   )
# 52 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp));
# 475 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                _2 
                   )
# 53 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp));
# 486 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Exp list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 54 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                                Apply(_1, _3) 
                   )
# 54 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp));
# 498 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                               [ ] 
                   )
# 57 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp list));
# 508 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                               [_1]   
                   )
# 58 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp list));
# 519 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Exp)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Exp list)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                                                               _1 :: _3 
                   )
# 59 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fsy"
                 : Exp list));
|]
# 532 "C:\Users\mire\Documents\MRH data\Kurser\02257-15\Project1\Handout\Test\ImpProgLang\Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 25;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Stm =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
let Exp lexer lexbuf : Exp =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 2))
let ExpList lexer lexbuf : Exp list =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 4))
let DecList lexer lexbuf : Dec list =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 6))
let Stm lexer lexbuf : Stm =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 8))
let StmList lexer lexbuf : Stm list =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 10))
let Dec lexer lexbuf : Dec =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 12))
