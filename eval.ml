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
  | ProcV of id list * body * env
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

(*
let rec lookupfun a  = function
    [] -> failwith ("runtime: fun " ^ a ^ " not fun exist")
  | (x, y, z) :: rest -> if x = a then (y, z) else lookupfun a rest;;
*)
(*
let rec extend f idlist explist =
  match (idlist, explist) with
    [], [] -> []
  | a :: b, c :: d ->
     (a, f c) :: (extend f b d)
  | _, _ -> failwith "parameter unmatch"
*)

(*
let rec extenddd a = function
    [] -> a
  | (id, _) :: rest -> extenddd ((id, ref UndefinedV) :: a) rest;;
let rec extendaaa eval a = function
    [] -> a
  | (id, x) :: rest -> extenddd ((id, eval x) :: a) rest;;
*)

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


let rec evalExp env = function
  | IntExp x -> IntV x
  | BoolExp x -> BoolV x
  | CharExp x -> CharV x
  | StringExp x -> StringV x
  | VarExp x -> !(lookup x env)
  | QuoteExp x -> evalQuote x
  | UnitExp -> UnitV
  | IfExp (c, a, b) ->
      let v = evalExp env c in
      evalExp env (match v with
          BoolV false -> b | _ -> a)

  | AndExp ls ->
      let rec loop result = function
        | [] -> result
        | a :: rest ->
            if truep result then loop (evalExp env a) rest else result
      in loop (BoolV true) ls
  | OrExp ls ->
      let rec loop result = function
        | [] -> result
        | a :: rest ->
            if truep result then result else loop (evalExp env a) rest
      in loop (BoolV false) ls

  | LambdaExp (ids, body) ->
     (* deflistは関数が適用された環境で評価されなければならない *)
      ProcV (ids, body, env)

  | ApplyExp (exp, args) ->
      let proc = evalExp env exp in
      let a = List.map (evalExp env) args in
      eval_apply  proc a
  | LetExp (binds, (defs, exp)) ->
      let a = extendlet env binds in
      let b = extendletrec a defs in
      evalExp b exp
  | LetrecExp (binds, (defs, exp)) ->
      let a = extendletrec env binds in
      let b = extendletrec a defs in
      evalExp b exp
  | SetExp (id, exp) ->
      let a = lookup id env in
      a := (evalExp env exp); UnitV
  | BeginExp explist ->
      let result = ref UnitV in
      List.iter (fun x -> result := evalExp env x) explist;
      !result

and eval_apply proc args =
  (match proc with
    ProcV (ids, (defs, exp), en) -> (* procには定義リストもある  *)
      let newenv = evalExplist en ids args in
      let newnewenv = extendletrec newenv defs in
      evalExp newnewenv exp
  | PrimV closure ->
      (* let s = List.map (evalExp env) args in *)
      closure args
  | UnboundV -> failwith "unbound proc!"
  | _ -> failwith "not proc")

and evalQuote = function
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
(*  | _ -> failwith "evalQuote" *)

and extendlet env  = function (* fold_right *)
    [] -> env
  | (id, x) :: rest ->
       (id, ref (evalExp env x)) :: extendlet env  rest
(*
and extendlet' env a = function (* fold_left *)
    [] -> a
  | (id, x) :: rest -> extendlet' env ((id, ref (evalExp env x)) :: a) rest
*)
and extendletrec env binds : env =  (* fold_left *)
  let rec ext a = function
      [] -> a
    | (id, _) :: rest -> ext ((id, ref UnboundV) :: a) rest in
  let b = ext env binds in
  List.iter (fun (id, ex) ->
    let a = lookup id b in
    a := evalExp b ex) binds;
  b

and evalExplist env ids args : env =
    match (ids, args) with
      [], [] -> env
    | id :: b, ex :: d ->
        (id, ref ( ex)) :: evalExplist env b d
    | _, _ -> failwith "parameter unmatch"
