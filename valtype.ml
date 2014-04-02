
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
  | ProcV  of id list * varid * 'a * 'a env
(*  | MacroV of id list * varid * 'a * 'a env *)
  | PrimV of ('a valtype list -> 'a valtype) 
  | PairV of 'a valtype ref * 'a valtype ref
  | EmptyListV
(*  | ProperListV of valtype list *)
(* list?の判定が定数時間で完了する。空リストを認めないとpair?の判定が簡単になる。ただしcdrの操作に注意が必要 *)
  | VectorV of 'a valtype array
  | UnitV
  | UnboundV
and 'a env = (id * 'a valtype ref) list

(* type proctype = env -> valtype *)

(* interpreter *)
let rec lookup a : ('a env -> 'a valtype ref) = function
    [] -> failwith ("runtime: var " ^ a ^ " not exist")
  | (x, v) :: rest -> if a = x then v else lookup a rest;;




(* applyのargsをrefに変換して環境に追加する *)
(*
let rec extend (env: 'a env) ids args  =
    match (ids, args) with
      [], [] -> env
    | id :: b, ex :: d ->
        (id, ref ex) :: extend env b d
    | _, _ -> failwith "parameter unmatch"
 *)

let arg_to_pair l =
  List.fold_right (fun x y -> PairV(ref x, ref y)) l EmptyListV
(*
let rec arg_to_pair = function
    [] -> EmptyListV
  | a :: rest -> PairV (ref a, ref (arg_to_pair rest))
 *)
(* applyのargsをrefに変換して環境に追加する *)
let rec extend_var (env : 'a env) ids varid args =
    match (ids, args) with
      [], x ->
        (match varid with
           Fixed -> env  (* todo:固定長のときはxにあまりがあってはいけない  *)
         | Vararg id -> let cc = arg_to_pair x in
	    (id, ref cc) :: env)
    | id :: b, ex :: d ->
        (id, ref ex) :: extend_var env b varid d
    | _, _ -> failwith "parameter unmatch"


let rec evalextend eval env (binds: (id * 'a) list) : 'a env =
  List.fold_right (fun (id, x) y -> (id, ref (eval x env)) :: y) binds env

(*
let rec evalextend eval env : (id * 'a) list -> 'a env = function (* fold_right *)
    [] -> env
  | (id, x) :: rest ->
       (id, ref (eval x env)) :: evalextend eval env rest
 *)

let extendletrec eval (env : 'a env) binds  =
  let rec ext = function
      [] -> env
    | (id, _) :: rest -> (id, ref UnboundV) :: ext rest in
  let newenv = ext binds in
  let rec loop e = function
      [] -> ()
    | (_, exp) :: rest  ->
        let (_, v) :: r = e in
        v := eval exp newenv; loop r rest
  in
  loop newenv binds;
  newenv


let rec evalSelf = function
  | Syntax.Int x -> IntV x
  | Syntax.Bool x -> BoolV x
  | Syntax.Char x -> CharV x
  | Syntax.String x -> StringV x


let rec evalQuote = function
    Syntax.Id s -> SymbolV s
  | Syntax.Int x -> IntV x
  | Syntax.Bool x -> BoolV x
  | Syntax.Char x -> CharV x
  | Syntax.String x -> StringV x
  | Syntax.List x ->
      List.fold_right (fun x y -> PairV (ref (evalQuote x), ref y)) x EmptyListV
(*
      let rec loop = function
        [] -> EmptyListV
      | a :: rest -> PairV (ref (evalQuote a), ref (loop rest)) in
       loop x
 *)
  | Syntax.Cons (x, y) ->
     PairV (ref (evalQuote x), ref (evalQuote y))
  | Syntax.Vector x -> VectorV (Array.map evalQuote (Array.of_list x))

let rec val_to_sexp = function
    IntV x -> Syntax.Int x
  | BoolV x -> Syntax.Bool x
  | CharV x -> Syntax.Char x
  | StringV x -> Syntax.String x
  | EmptyListV -> Syntax.List []
  | SymbolV x -> Syntax.Id x
  | PairV (x, y) ->
     (match val_to_sexp !y with
        Syntax.List a -> Syntax.List (val_to_sexp !x :: a)
      | a -> Syntax.Cons (val_to_sexp !x, a)
     )
  | x -> failwith "can not match sexp" 
