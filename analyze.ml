
(*
 sicp 4.1.7
*)

(*
  2014/3/1
    -rectypes をつけるとコンパイルできる
 *)


open Parser

open Valtype



type 'a proctype = 'a env -> 'a valtype





let rec analyzeExp  : exp -> 'a proctype = function
  | SelfEvalExp sexp -> 
      let result = evalSelf sexp in fun _ -> result
  | UnspecifiedExp -> fun _ -> UnitV
  | VarExp x ->
     fun env ->
     (match !(lookup x env) with
        UnboundV -> failwith ("runtime: var " ^ x ^ " Unbound")
      | x -> x
     )
  | QuoteExp x ->
      let result = evalQuote x in fun _ -> result
  | QuasiQuoteExp x ->
     let rec evalQuasi v env =
       (match v with
         S x -> evalQuote x
       | Unquote x -> analyzeExp x env
       | UnquoteSplice x ->
          failwith "splice not in list"
       | Nil -> EmptyListV
       | P (x, y) ->
          let rec splice rest = function
              EmptyListV -> evalQuasi rest env
            | PairV (a, b) -> (PairV (a, ref (splice rest !b)))
            | x -> failwith "splice not list"
             in
          (match x with
             UnquoteSplice x ->
             (match y with
              | Nil -> analyzeExp x env
              | _ -> splice y (analyzeExp x env)
             )
           | _ -> PairV (ref (evalQuasi x env), ref (evalQuasi y env))
          )
       ) in
     evalQuasi x

  | IfExp (c, a, b) ->
      let pred  = analyzeExp c
      and conseq = analyzeExp a
      and alt = analyzeExp b in
      (fun env ->
       (match pred env with
        | BoolV false -> alt | _ -> conseq) env
      )
  | AndExp ls ->
     let args = List.map analyzeExp ls in
     fun env ->
      let rec loop result = function
        | [] -> result
        | p :: rest ->
            (match result with
               BoolV false -> result | _ -> loop (p env) rest
            ) in
      loop (BoolV true) args
 
  | OrExp ls ->
     let args = List.map analyzeExp ls in
     fun env ->
      let rec loop result = function
          [] -> result
        | p :: rest ->
            (match result with
              BoolV false -> loop (p env) rest | e -> e
            ) in
      loop (BoolV false) args

  | LambdaExp (ids, varid, exp) ->
     let proc = analyzeExp exp in
     fun env -> ProcV (ids, varid, proc, env)
(*
  | MacroExp (ids, vararg, exp) ->
     let proc = analyzeExp exp in
     fun env -> MacroV (ids, vararg, proc, env)
 *)
  | ApplyExp (exp, args) ->
     let proc = analyzeExp exp
     and a = List.map analyzeExp args in
     fun env ->
       let aa = List.map (fun p -> p env) a in 
       eval_apply (proc env) aa
  | MacroAppExp (id, sexps) ->
     let args = List.map evalQuote sexps in
     fun env ->
     let m = !(lookup id env) in
     let v = eval_apply m args in
     let ex = parseExp (val_to_sexp v) in
     analyzeExp ex env

(*
  | LetExp (binds, (defs, exp)) ->
     let bb = List.map (fun (id, ex) -> (id, analyzeExp ex)) binds
     and dd = List.map (fun (id, ex) -> (id, analyzeExp ex)) defs
     and exp = analyzeExp exp in
     fun env ->
      let a = evalextend (fun e -> e) env bb in
      let b = extendletrec a dd in
      exp b
 *)
(*
  | NamedLetExp (id, binds, body) ->
      let (ids, args) = List.split binds
      and pid = analyzeExp (VarExp id) in
      let arr = List.map analyzeExp args
      and fn = LambdaExp (ids, Fixed, body) in
      let pfn = [id, analyzeExp fn] in
      fun env ->
        let a = extendletrec env pfn in
        (* todo: pid a ではなく lookup id a でいいかもしれない  *)
        eval_apply (pid a) (List.map (fun p -> p env) arr)
 *)
(*
  | LetrecExp (binds, (defs, exp)) ->
     let e = analyzeExp exp
     and bb = List.map (fun (id, ex) -> (id, analyzeExp ex)) binds 
     and dd = List.map (fun (id, ex) -> (id, analyzeExp ex)) defs in
     fun env ->
       let a = extendletrec env bb in
       let b = extendletrec a dd in
       e b
 *)
  | LetrecExp (binds, exp) ->
     let e = analyzeExp exp
     and bb = List.map (fun (id, ex) -> (id, analyzeExp ex)) binds in
     fun env ->
       let a = extendletrec env bb in
       e a

  | CondClauseExp x -> analyze_cond x
  | SetExp (id, exp) ->
     let e = analyzeExp exp in
     fun env ->
       let a = lookup id env in
       a := (e env); UnitV

  | SeqExp (a, b) ->
     let proc1 = analyzeExp a and proc2 = analyzeExp b in
     fun env -> proc1 env; proc2 env;

  | DoExp ((vars, inits, steps), test, exp, cmds) ->
     let inits = List.map analyzeExp inits
       and steps = List.map analyzeExp steps
       and test = analyzeExp test
       and exp = analyzeExp exp
       and cmds = List.map analyzeExp cmds in
     fun env ->
     let rec loop args =
       let newenv = extend_var env vars Fixed args in
       (match test newenv with
	 BoolV false ->
           List.map (fun p -> p newenv) cmds;
           loop (List.map (fun p -> p newenv) steps)
       | _ -> exp newenv
       ) in
     loop (List.map (fun p -> p env) inits)

and analyze_cond = function
    ARROW (cond, conseq, alt) ->
      let pcond = analyzeExp cond
      and pcon = analyzeExp conseq
      and palt = analyzeExp alt in
      fun env ->
        (match pcond env with
           BoolV false -> palt env
         | e -> eval_apply (pcon env) [e]
        )
  | VAL (cond, alt) ->
     let pcond = analyzeExp cond
     and palt = analyzeExp alt in
     fun env ->
       (match pcond env with
        | BoolV false -> palt env | e -> e)

(* unused  *)
and analyze_seq exps =
  let sequentially proc1 proc2 env =
    proc1 env; proc2 env in
  let rec loop proc = (function
        [] -> proc (* proc自体が proctypeのため,funで包まなくてよい *)
      | a :: b -> loop (sequentially proc a) b) in
  let procs = List.map analyzeExp exps in
  (match procs with
     [] -> failwith "empty seq"
   | a :: b -> loop a b
  )

and eval_apply proc args =
  (match proc with
     ProcV (ids, varid, pexp, env) ->
      let newenv =   extend_var env ids varid args in
      (*let newnewenv = extendletrec newenv pdefs in*)
      pexp newenv
   | PrimV closure ->
      closure args
   | _  -> failwith "not proc"
  )


and extendletrec env =
  Valtype.extendletrec (fun exp -> exp) env

