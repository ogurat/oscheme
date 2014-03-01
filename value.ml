
open Parser

(*
runtime 
 environment and values
 *)


type 'a valtype =
    IntV of int
  | BoolV of bool
  | CharV of char
  | StringV of string
  | SymbolV of id
  | ProcV of id list * (id * 'a) list * 'a * 'a env
  | PrimV of ('a valtype list -> 'a valtype) 
  | PairV of 'a valtype ref * 'a valtype ref
  | EmptyListV
(*  | ProperListV of valtype list *)
(* list?の判定が定数時間で完了する。空リストを認めないとpair?の判定が簡単になる。ただしcdrの操作に注意が必要 *)
(*  | VectorV of 'a valtype array *)
  | UnitV
  | UnboundV
and 'a env = (id * 'a valtype ref) list

(* type proctype = env -> valtype *)

(* interpreter *)
let rec lookup a : ('a env -> 'a valtype ref) = function
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

(*
let rec appendenv env = function
    [] -> env
  |  a :: rest -> appendenv (a :: env) rest;;
 *)

(* applyのargsをrefに変換して環境に追加する *)
let rec extend env ids args  =
    match (ids, args) with
      [], [] -> env
    | id :: b, ex :: d ->
        (id, ref ex) :: extend env b d
    | _, _ -> failwith "parameter unmatch"


and evalextend eval env : (id * exp) list -> 'a env = function (* fold_right *)
    [] -> env
  | (id, x) :: rest ->
       (id, ref (eval env x)) :: evalextend eval env  rest

