module AstToTree

open System
open TreeGenerator
open AST

let rec translateExp e = 
    match e with
    |Int i                                  -> Node(("Int"),[Node(sprintf "%i" i, [])])
    |Bool b                                 -> Node(("Bool"), [Node (sprintf "%b" b, [])])
    |String s                               -> Node(("String"),[Node(s, [])])
    |Var v                                  -> Node(("Var " + v), [])
    |ContOf exp                             -> Node(("ContOf"), [translateExp exp])
    |Apply (s, (el : List<Exp>))            -> Node(("Apply " + s), List.map translateExp el)
and translateDec d =
    match d with
    |VarDec (s, e)                          -> Node("Dec " + s, [translateExp e]) 
    |ProcDec (s, sl : list<string>, stm)    -> Node("ProcDec " + s + "," + sprintf "%A" sl, [translateStm stm]) 
    |RecProcDec (s, sl : list<string>, stm) -> Node("RecProcDec " + s + "," + sprintf"%A" sl, [translateStm stm]) 
and translateStm t = 
    match t with
    | Block ((list:List<Dec>),stm)          -> Node("Block", (List.map translateDec list) @ [(translateStm stm)]) 
    | Asg (e1,e2)                           -> Node("Asg", [(translateExp e1); (translateExp e2)]) 
    | PrintLn e                             -> Node("PrintLn", [translateExp e]) 
    | Seq (list : List<Stm>)                -> Node("Seq", List.map translateStm list) 
    | While (e, stm)                        -> Node("While", [(translateExp e); (translateStm stm)]) 
    | IfElse (e, stm1, stm2)                -> Node("IfElse", [(translateExp e); (translateStm stm1); (translateStm stm2)]) 
    | If (e, stm)                           -> Node("If", [(translateExp e); (translateStm stm)]) 
    | Call e                                -> Node("Call", [(translateExp e)]) 
    | Return e                              -> Node("Return", [(translateExp e)]) 