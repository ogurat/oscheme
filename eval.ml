(* oscheme eval.ml *)

open Parser

open Valtype



let rec evalExp env = function
  | SelfEvalExp sexp -> evalSelf sexp
  | UnitExp -> UnitV
  | VarExp x ->
     (match !(lookup x env) with
        UnboundV -> failwith ("runtime: var " ^ x ^ " Unbound")
      | x -> x
     )
  | QuoteExp x -> evalQuote x
  | QuasiQuoteExp x -> 
     let rec evalQuasi = function
         S x -> evalQuote x
       | Unquote x -> evalExp env x
       | UnquoteSplice x ->
          failwith "splice not in list"
       | Empty -> EmptyListV
       | P (x, y) ->
          let rec splice rest = function
              EmptyListV -> evalQuasi rest
            | PairV (a, b) -> (PairV (a, ref (splice rest !b)))
            | x -> failwith "splice not list"
             in
          (match x with
             UnquoteSplice x ->
             (match y with
              | Empty -> evalExp env x
              | _ -> splice y (evalExp env x)
             )
           | _ -> PairV (ref (evalQuasi x), ref (evalQuasi y))
          )
(*
       | L x ->
          let rec loop = function
              [] -> EmptyListV
            | a :: rest ->
               (match a with
                  UnquoteSplice x ->
                  splice rest (evalExp env x)
                | _ -> PairV (ref (evalQuasi a), ref (loop rest))
               )
          and splice rest = function
              EmptyListV -> loop rest
            | PairV (a, b) -> (PairV (a, ref (splice rest !b)))
            | _ -> failwith "splice not list"
             in
          loop x
 *)
     in evalQuasi x

  | IfExp (c, a, b) ->
      evalExp env (match evalExp env c with
          BoolV false -> b | _ -> a)

  | AndExp ls ->
      let rec loop result = function
        | [] -> result
        | a :: rest ->
            (match result with
               BoolV false -> BoolV false
             | _ -> loop (evalExp env a) rest
            )
      in loop (BoolV true) ls
  | OrExp ls ->
      let rec loop result = function
        | [] -> result
        | a :: rest ->
            (match result with
               BoolV false -> loop (evalExp env a) rest
             | e -> e
            )
      in loop (BoolV false) ls

  | LambdaExp (ids, varid, exp) ->
      ProcV (ids, varid, exp, env)
  | MacroExp (ids, vararg, exp) ->
      MacroV (ids, vararg, exp, env)
  | ApplyExp (exp, args) ->
      let proc = evalExp env exp
      and a = List.map (evalExp env) args in
      eval_apply proc a
  | MacroAppExp (id, sexps) ->
     let args = List.map evalQuote sexps in
     (match !(lookup id env) with
      | MacroV (ids, varid, exp, en) ->
         let newenv = extend_var en ids varid args in
         let v = evalExp newenv exp in
         evalExp env (parseExp (val_to_sexp v))
      | _ -> failwith "not macro"
     )
(*
  | LetExp (binds, (defs, exp)) ->
      let a = evalextend (fun e en -> evalExp en e) env binds in
      let b = extendletrec a defs in
      evalExp b exp
 *)
(*
  | NamedLetExp (id, binds, body) ->
      let (ids, args) = List.split binds in
      let fn = LambdaExp (ids, Fixed, body) in
      let a = extendletrec env [id, fn] in
      eval_apply (evalExp a (VarExp id)) (List.map (evalExp env) args)
 *)
(*
  | LetrecExp (binds, (defs, exp)) ->
      let a = extendletrec env binds in
      let b = extendletrec a defs in
      evalExp b exp
 *)
  | LetrecExp (binds,  exp) ->
      let a = extendletrec env binds in
      evalExp a exp
  | CondClauseExp x -> eval_cond env x
  | SetExp (id, exp) ->
      let a = lookup id env in
      a := (evalExp env exp); UnitV
(*
  | BeginExp exps ->
     let rec loop = function
         [] -> UnitV
       | [x] -> evalExp env x
       | x :: rest -> evalExp env x; loop rest in
     loop exps
 *)
  | SeqExp (a, b) ->
     evalExp env a; evalExp env b

  | DoExp ((vars, inits, steps), test, exp, cmds) ->
     let rec loop args =
       let newenv = extend_var env vars Fixed args in
       (match evalExp newenv test with
         BoolV false ->
           List.map (evalExp newenv) cmds;
           loop (List.map (evalExp newenv) steps)
       | _ -> evalExp newenv exp
       ) in
     loop (List.map (evalExp env) inits)

and eval_cond env = function
    ARROW (cond, ret, alt) ->
     (match evalExp env cond with
        BoolV false -> evalExp env alt
      | e -> eval_apply (evalExp env ret) [e]
     )
  | VAL (cond, alt) ->
     (match evalExp env cond with
      | BoolV false -> evalExp env alt | e -> e
     )

and eval_apply proc args =
  (match proc with
    ProcV (ids, varid, exp, env) ->
     let newenv = extend_var env ids varid args in
     evalExp newenv exp


  | PrimV closure ->
     closure args
  | UnboundV -> failwith "unbound proc!"
  | _ -> failwith "not proc"
  )


and extendletrec env =
  Valtype.extendletrec (fun exp en -> evalExp en exp) env

