(* Interpreter for a simple WHILE-language. Michael R. Hansen 03-01-2014 *)
(* Based on a natural semantics of WHILE                                 *)

(* Remember to regenerate the parser and the lexer using the commands 
   in README.txt if you modified the parser and lexer                    *)

module Interpreter 

open System
open AST

type Location = int
type Value    = | IntVal of int 
                | BoolVal of bool 
                | StringVal of string 
                | Reference of Location 
                | Primitive of (List<Value> -> Value)
and Env       = Map<string,Value>


type Closure =  List<string> * Env * Stm

type Content = SimpVal of Value | Proc of Closure |  ArrayCnt of Value [];;

type Store  = Map<Location,Content>  
  
let closureOf(ps,st) env = (ps, env, st)

// nextLoc() generates the next available location
let nextLoc: unit -> int =  let n = ref 0
                            let f x = (n := !n+1; !n)
                            f

// exp: Exp -> Env -> Store -> Value * Store 
let rec exp e (env:Env) (store:Store) = 
    //printfn "e %A" e   
    match e with
    | Var v       -> match Map.find v env with
                     | Reference loc as refl -> (refl,store)
                     | IntVal i              -> printfn "%s" (string i) ; failwith "errorXXX"
                     | _                     -> failwith "errorYYY"
    | ContOf er    -> match exp er env store with
                      | (Reference loc,store1) -> match Map.find loc store1 with 
                                                  | SimpVal res -> (res,store1)
                                                  | _           -> failwith "error Z"
                      | _                   -> failwith "errors"

    | Apply(f,es) -> let (vals, store1) = expList es env store 
                     //printfn "%A" (Map.find f env)
                     match Map.find f env with 
                     | Primitive f   -> (f vals, store1)
                     | Reference loc -> 
                            match Map.find loc store1 with 
                            |Proc(ns,e,st) ->   
                                let vars = List.zip ns es
                                let funcs acc ((name:string),value) = Map.add name (fst(exp value acc store1)) acc
                                let env2 = (List.fold funcs e vars)
                                let store2 = Map.add loc (Proc (closureOf(ns,st) env2)) store1
                                         

                                //printfn "---> hey" 
                                match stm st env2 store2 with
                                    | (Some k,store2)  ->  (k,store2)
                                    | (None, store2) -> (StringVal "" , store2)
                                    | _ ->failwith "error GssG"
                            
                            | _           -> failwith "error GG"
                     | _              -> failwith "type error"          
                                                               
    | Int i       -> (IntVal i, store)
    | Bool b      -> (BoolVal b,store)  
    | String s    -> (StringVal s,store)

and expList es env store = 
    match es with 
    | []       -> ([],store)
    | e::erest -> let (res1, store1) = exp e env store
                  let (ress, store2) = expList erest env store1
                  (res1::ress, store2)  

 
// stm: Stm -> Env -> Store -> option<Value> * Store
and stm st (env:Env) (store:Store) = 
    //printfn "st %A" st
    match st with 
    | Asg(el,e) -> let (res,store1) = exp e env store
                   let (resl, store2) = exp el env store1
                   match resl with 
                   | Reference loc -> (None, Map.add loc (SimpVal res) store2) 
                   | _                               -> failwith "type error"
                   
         
    | PrintLn e -> match exp e env store with
                   | (StringVal s,store1) -> (printfn "%s" s; (None,store1))
                   | _                    -> failwith "error"                  
                                                                 
                                           
    | Seq []        -> (None,store)
    | Seq (st::sts) -> match stm st env store with 
                       | (None,store1)   -> stm (Seq sts) env store1
                       | result       -> result

    | While(e,st1)  -> let (res, store1) = exp e env store
                       //printfn " %A" (res)
                       match res with 
                       | BoolVal true  -> 
                                          match stm st1 env store1 with
                                          | (None,store2) -> stm st env store2
                                          | result     -> result
                       | BoolVal false -> (None, store1)
                       | _             -> failwith "type error"

    | If(e,st1)     -> let (res, store1) = exp e env store
                      // printfn "asd %A" (res)
                       match res with 
                       | BoolVal true  ->  stm st1 env store1
                       | BoolVal false ->  (None , store1)
                       | _             -> failwith "IF error"
     |IfElse(e,st1,st2) -> let (res, store1) = exp e env store
                      // printfn "asd %A" (res)
                           match res with 
                           | BoolVal true  ->  stm st1 env store1
                           | BoolVal false ->  stm st2 env store1
                           | _             -> failwith "IF error"

   
    | Call(e) ->  match exp e env store with  (k,store1) ->  (None,store1)
                               

    | Block(ds,st1) -> let (env1,store1) = decList ds env store 
                       stm st1 env1 store1 
    | Return(e) ->  
                    let (res, store1) = exp e env store
                    //printfn  "aaaaaaaa %A" (res,store1)
                    (Some res, store1)
    
and decList ds env store = 
    match ds with
    | []       -> (env,store)
    | d::drest -> let (env1,store1) = dec d env store
                  decList drest env1 store1

and dec d env store =
    match d with 
    | VarDec(s,e) -> let loc = nextLoc()
                     match exp e env store with
                     | (IntVal _ as res, store1)  
                     | (BoolVal _ as res, store1) 
                     | (StringVal _ as res, store1)  
                                                 -> let env2 = Map.add s (Reference loc) env
                                                    let store2 = Map.add loc (SimpVal res) store1
                                                    (env2, store2)
                     | _                         -> failwith "error"   
     | ProcDec(s,ls,st1) -> let loc = nextLoc() 
                            let env2 = Map.add s (Reference loc) env
                            let store2 = Map.add loc (Proc (closureOf(ls,st1) env)) store
                            (env2, store2)
     | RecProcDec(s,ls,st1) -> let loc = nextLoc()
                               let env2 = Map.add s (Reference loc) env
                               let store2 = Map.add loc (Proc (closureOf(ls,st1) env2)) store
                               (env2, store2)                             
;;

