(* oscheme eval.ml *)

open Parser


type env = (id * valtype ref) list
and
valtype =
    IntV of int
  | BoolV of bool
  | CharV of char
  | StringV of string
  | SymbolV of id
  | ProcV of id list * body * env  (* body: (id * exp) list * exp  *)
  | PrimV of (valtype list -> valtype) 
  | PairV of valtype ref * valtype ref
  | EmptyListV
(*  | ProperListV of valtype list *)
(* list?の判定が定数時間で完了する。空リストを認めないとpair?の判定が簡単になる。ただしcdrの操作に注意が必要 *)
  | VectorV of valtype array
  | UnitV
  | UnboundV


(* interpreter *)
let rec lookup a : (env -> valtype ref) = function
    [] -> failwith ("runtime: var " ^ a ^ " not exist")
  | (x, v) :: rest -> if a = x then v else lookup a rest;;



(* schemeの値の真偽を評価する bool値のfalseだけが偽と評価されその他は真と評価される *)
let truep = function
  BoolV false -> false
| _           -> true


(*
primis = ("#t", ref (BoolV true)) :: primis;;
primis = ("#f", ref (BoolV false)) :: primis;;
  *)

let rec appendenv env = function
    [] -> env
  |  a :: rest -> appendenv (a :: env) rest;;

(* applyのargsをrefに変換して環境に追加する *)
let rec extendExplist env ids args : env =
    match (ids, args) with
      [], [] -> env
    | id :: b, ex :: d ->
        (id, ref ex) :: extendExplist env b d
    | _, _ -> failwith "parameter unmatch"


let rec evalQuote = function
    Syntax.Id s -> SymbolV s
  | Syntax.Bool x -> BoolV x
  | Syntax.Char x -> CharV x
  | Syntax.String x -> StringV x
  | Syntax.Int x -> IntV x
  | Syntax.List x ->
      let rec loop = function
        [] -> EmptyListV
      | a :: rest -> PairV (ref (evalQuote a), ref (loop rest)) in
      loop x


let rec evalExp env = function
  | IntExp x -> IntV x
  | BoolExp x -> BoolV x
  | CharExp x -> CharV x
  | StringExp x -> StringV x
  | VarExp x -> !(lookup x env)
  | QuoteExp x -> evalQuote x
  | UnitExp -> UnitV
  | IfExp (c, a, b) ->
      evalExp env (match evalExp env c with
          BoolV false -> b | _ -> a)

  | AndExp ls ->
      let rec loop = function
        | [] -> BoolV true
	| [a] -> evalExp env a
        | a :: rest ->
            (match evalExp env a with
	       BoolV false -> BoolV false
	     | _ -> loop rest)
      in loop ls
  | OrExp ls ->
      let rec loop result = function
        | [] -> result
        | a :: rest ->
            (match result with
	       BoolV false -> loop (evalExp env a) rest
	     | e ->  e)
      in loop (BoolV false) ls

  | LambdaExp (ids, body) ->
     (* deflistは関数が適用された環境で評価されなければならない *)
      ProcV (ids, body, env)

  | ApplyExp (exp, args) ->
      let proc = evalExp env exp
      and a = List.map (evalExp env) args in
      eval_apply proc a
  | LetExp (binds, (defs, exp)) ->
      let a = extendlet env binds in
      let b = extendletrec a defs in
      evalExp b exp
  | NamedLetExp (id, binds, body) ->
      let (ids, args) = List.split binds in
      let fn = LambdaExp (ids, body) in
      let a = extendletrec env [id, fn] in
      eval_apply (evalExp a (VarExp id)) (List.map (evalExp env) args)
  | LetrecExp (binds, (defs, exp)) ->
      let a = extendletrec env binds in
      let b = extendletrec a defs in
      evalExp b exp
  | CondClauseExp x ->
      (match x with
	FUN (cond, ret, alt) ->
	 (match evalExp env cond with
	    BoolV false -> evalExp env alt
	  | e -> eval_apply (evalExp env ret) [e])
      | VAL (cond, alt) ->
	 (match evalExp env cond with
	  | BoolV false -> evalExp env alt | e -> e)
      )
  | SetExp (id, exp) ->
      let a = lookup id env in
      a := (evalExp env exp); UnitV
  | BeginExp exps ->
     let rec loop = function
	 [] -> UnitV
       | [x] -> evalExp env x
       | x :: rest -> evalExp env x; loop rest in
     loop exps
(*
      let result = ref UnitV in
      List.iter (fun x -> result := evalExp env x) explist;
      !result
*)

and eval_apply proc args =
  (match proc with
    ProcV (ids, (defs, exp), en) ->
      let newenv = extendExplist en ids args in
      let newnewenv = extendletrec newenv defs in
      evalExp newnewenv exp
  | PrimV closure ->
      closure args
  | UnboundV -> failwith "unbound proc!"
  | _ -> failwith "not proc")


and extendlet env : (id * exp) list -> env = function (* fold_right *)
    [] -> env
  | (id, x) :: rest ->
       (id, ref (evalExp env x)) :: extendlet env  rest
(*
and extendlet' env a = function (* fold_left *)
    [] -> a
  | (id, x) :: rest -> extendlet' env ((id, ref (evalExp env x)) :: a) rest
*)

and extendletrec env binds : env =
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

and extendletrec' env binds : env =  (* fold_left *)
  let rec ext a = function
      [] -> a
    | (id, _) :: rest -> ext ((id, ref UnboundV) :: a) rest in
  let newenv = ext env binds in
  List.iter (fun (id, ex) ->
	     let a = lookup id newenv in
	     a := evalExp newenv ex)
	    binds;
  newenv
