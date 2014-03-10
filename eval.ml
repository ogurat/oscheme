(* oscheme eval.ml *)

open Parser

open Valtype



let rec evalExp env = function
  | SelfEvalExp sexp -> evalSelf sexp
  | UnitExp -> UnitV
  | VarExp x -> !(lookup x env)
  | QuoteExp x -> evalQuote x
  | IfExp (c, a, b) ->
      evalExp env (match evalExp env c with
          BoolV false -> b | _ -> a)

  | AndExp ls ->
      let rec loop result = function
        | [] -> result
        | a :: rest ->
            (match result with
               BoolV false -> BoolV false
             | _ -> loop (evalExp env a) rest)
      in loop (BoolV true) ls
  | OrExp ls ->
      let rec loop result = function
        | [] -> result
        | a :: rest ->
            (match result with
               BoolV false -> loop (evalExp env a) rest
             | e -> e)
      in loop (BoolV false) ls

  | LambdaExp (ids, varid, (defs, exp)) ->
     (* deflistは関数が適用された環境で評価されなければならない *)
      ProcV (ids, varid, defs, exp, env)

  | ApplyExp (exp, args) ->
      let proc = evalExp env exp
      and a = List.map (evalExp env) args in
      eval_apply proc a
  | LetExp (binds, (defs, exp)) ->
      let a = evalextend evalExp env binds in
      let b = extendletrec a defs in
      evalExp b exp
  | NamedLetExp (id, binds, body) ->
      let (ids, args) = List.split binds in
      let fn = LambdaExp (ids, Fixed, body) in
      let a = extendletrec env [id, fn] in
      eval_apply (evalExp a (VarExp id)) (List.map (evalExp env) args)
  | LetrecExp (binds, (defs, exp)) ->
      let a = extendletrec env binds in
      let b = extendletrec a defs in
      evalExp b exp
  | CondClauseExp x -> eval_cond env x
  | SetExp (id, exp) ->
      let a = lookup id env in
      a := (evalExp env exp); UnitV
  | BeginExp exps ->
     let rec loop = function
         [] -> UnitV
       | [x] -> evalExp env x
       | x :: rest -> evalExp env x; loop rest in
     loop exps
  | SeqExp (a, b) ->
     evalExp env a; evalExp env b

and eval_cond env = function
    ARROW (cond, ret, alt) ->
     (match evalExp env cond with
        BoolV false -> evalExp env alt
      | e -> eval_apply (evalExp env ret) [e])
  | VAL (cond, alt) ->
     (match evalExp env cond with
      | BoolV false -> evalExp env alt | e -> e)

and eval_apply proc args =
  (match proc with
    ProcV (ids, varid, defs, exp, env) ->
      let newenv = extend_var env ids varid args in
      let newnewenv = extendletrec newenv defs in
      evalExp newnewenv exp
  | PrimV closure ->
      closure args
  | UnboundV -> failwith "unbound proc!"
  | _ -> failwith "not proc")

and extendletrec env =
  Valtype.extendletrec (fun exp env -> evalExp env exp) env
(*
and extendletrec env binds : 'a env =
  let rec ext = function
      [] -> env
    | (id, _) :: rest -> (id, ref UnboundV) :: ext rest in
  let newenv = ext binds in
  let rec loop e = function
      [] -> ()
    | (_, exp) :: rest  ->
        let (_, v) :: r = e in
        v := evalExp newenv exp; loop r rest
  in
  loop newenv binds;
  newenv
 *)
