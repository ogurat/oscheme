

open Syntax
open Valtype
open Parser


let rec expandExp env = function
(*
  | SelfEvalExp sexp -> evalSelf sexp
  | UnitExp -> UnitV
  | VarExp x ->
     (match !(lookup x env) with
        UnboundV -> failwith ("runtime: var " ^ x ^ " Unbound")
      | x -> x
     )
  | QuoteExp x -> evalQuote x
 *)
  | ConsExp (a, b) -> ConsExp (expandExp env a, expandExp env b)
  | IfExp (c, a, b) ->
     IfExp (expandExp env c, expandExp env a, expandExp env b)
(*
  | OrExp ls ->
     OrExp (List.map (expandExp env) ls)
 *)
  | LambdaExp (ids, varid, exp) ->
      LambdaExp (ids, varid, expandExp env exp)

  | MacroExp (ids, varid, exp) ->
      MacroExp (ids, varid, expandExp env exp)

  | ApplyExp (exp, args) ->
      let proc = expandExp env exp
      and a = List.map (expandExp env) args in
      ApplyExp (proc,a)
 
  | MacroAppExp (id, sexps) ->
     let args = List.map evalQuote sexps
     and proc = !(lookup id env) in
     let v = Eval.eval_apply proc args in
     expandExp env (parseExp (val_to_sexp v))

  | LetrecExp (binds, exp) ->
      let a = Eval.extendletrec env binds
      and bin = List.map (fun (id, e) -> (id, expandExp env e)) binds in
      LetrecExp (bin, expandExp a exp)

  | CondArrow (test, ret, alt) ->
     CondArrow (expandExp env test,
                           expandExp env ret,
                           expandExp env alt)
  | CondVal (test, alt) ->
     CondVal (expandExp env test,
                         expandExp env alt)

  | SetExp (id, exp) ->
     SetExp (id, expandExp env exp)

  | SeqExp (a, b) ->
     SeqExp (expandExp env a, expandExp env b)

  | DoExp ((vars, inits, steps), test, exp, cmds) ->
     let inits = List.map (expandExp env) inits
     and steps = List.map (expandExp env) steps
     and cmds = List.map (expandExp env) cmds in
     DoExp((vars, inits, steps), 
           expandExp env test, expandExp env exp, cmds)

  | x -> x

(*
and expand_cond env = function
    ARROW (cond, ret, alt) ->
     CondClauseExp (ARROW (expandExp env cond,
                           expandExp env ret,
                           expandExp env alt))
  | VAL (cond, alt) ->
     CondClauseExp (VAL (expandExp env cond,
                         expandExp env alt))
 *)
