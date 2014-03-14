(* oschme scheme.ml *)

open Parser
open Valtype



(* primitives *)


let rec printval = function
    IntV i -> string_of_int i
  | BoolV x ->  (if x then "#t" else "#f")
(*  | CharV x -> string_of_char x *)
  | SymbolV x ->  (x)
  | StringV x ->  x (* "\"" ^ x ^ "\"" *)
  | ProcV (args, _, _, env) ->
     let rec pprest = function
	 [] -> ""
       | a :: b ->  " " ^ a ^ pprest b in
     "#proc:" ^ (
       (function
	   [] -> "()"
	 | x :: rest -> "(" ^  x ^ pprest rest ^ ")")
	 args)
  | PrimV _ ->  "primitive"
  | PairV (a, b) ->  "(" ^ printval !a ^ pppair !b ^ ")"
  | EmptyListV ->  "()"
  | UnboundV ->  "*unbound*"
  | UnitV -> "#void"

and pppair = function(* PairVの第2要素 *)
    EmptyListV -> ""
  | PairV (a, b) ->  " " ^ printval !a ^ pppair !b
  | arg -> " . " ^ printval arg



(* schemeの値の真偽を評価する bool値のfalseだけが偽と評価されその他は真と評価される *)
let truep = function
  BoolV false -> false
| _           -> true



let read () =
  let buf = Lexing.from_channel stdin in
  (* let buf = Lexing.from_string (input_line  stdin) in *) (* 本来 1s式をよむ。次の readで残りのs  *)
  let sexp = Sparser.sexpdata Lexer.main buf in
     evalQuote sexp

let chareq a b = BoolV (a = b)

let eqp x y =
  match (x, y) with
  | (BoolV true, BoolV true) 
  | (BoolV false, BoolV false) ->  true
  | (SymbolV s, SymbolV s2) -> s = s2
  | (EmptyListV, EmptyListV) ->  true
  | StringV a, StringV b -> a == b
(*  | VectorV a, VectorV b -> a == b *)
  | _ ->  false

let eqvp x y =
  match (x, y) with
  | a,b when (eqp a b) -> true
  | (IntV x, IntV y) -> x = y
  | (CharV c, CharV d) -> c = d
  | _ ->  false

let rec equalp x y =
  match (x, y) with
  | a,b when (eqvp a b) ->  true
  | PairV (a, b), PairV (c, d) -> (equalp !a !c) && (equalp !b !d)
(*  | VectorV a, VectorV b -> false *)
  | _ ->  false


(*
let add (ls :valtype list) =
  let rec apply = function
    | [IntV i] -> i
    | IntV a :: tl -> a + apply tl 
    | _ -> failwith "Arity mismatch: +"
  in IntV (apply ls);;

let multi (ls :valtype list) = 
  let rec apply = function
    | [IntV i] -> i
    | IntV a :: tl -> a * apply tl 
    | _ -> failwith "Arity mismatch: +"
  in IntV (apply ls)
 *)
let minus ls =
  let rec apply = function
    | [IntV i; IntV j] -> i - j
    | _ -> failwith "Arity mismatch: -"
  in IntV (apply ls)

let booleanp = function
    BoolV _ ->  true
  | _ -> false

let not v =
    if (truep v) then false else true

let eq x y =
  match (x,y) with
    | IntV i, IntV j -> i = j 
    | _ -> false

(*
let lt ls =
  let rec apply = function
    | [IntV i; IntV j] -> i < j 
    | _ -> failwith "Arity mismatch: ="
  in BoolV (apply ls)

let gt ls =
  let rec apply = function
    | [IntV i; IntV j] -> i > j 
    | _ -> failwith "Arity mismatch: ="
  in BoolV (apply ls)
 *)
let numberp = function
    IntV _ -> true
  | _ -> false


let pairp = function
    PairV _ -> true
  | _ -> false

let cons a b =
  PairV (ref a, ref b)

let car = function
    PairV (x, _) -> !x
  | _ -> failwith "Arity mismatch: car"

let cdr = function 
    PairV (_, x) -> !x
  | _ -> failwith "Arity mismatch: cdr"

let setcar v = function
    PairV (a, _) -> a := v; UnitV
  | _ -> failwith "Arity mismatch: set-car!"

let setcdr v = function
    PairV (_, a) -> a := v; UnitV
  | _ -> failwith "Arity mismatch: set-cdr!"

let nullp = function
    EmptyListV -> true
  | _ ->  false

let rec listp = function
    EmptyListV -> true
  | PairV (_, b) -> listp !b
  | _ ->  false
 
let rec makelist = function
    [] -> EmptyListV
  | a :: rest -> cons a (makelist rest) 
(*
let rec length r = function
  | arg -> if nullp arg then r else length (r+1) (cdr arg);;
*)
let rec length r = function
    EmptyListV -> r
  | PairV (_, rest) -> length (r + 1) !rest
  | _ -> failwith "Arity mismatch: length"
  
(*
let rec append_impl a b =
  if nullp a then
    b
  else
    cons (car a) (append_impl (cdr a) b)
*)
let rec append_impl l1 l2 =
  match l1 with
    EmptyListV -> l2
  | PairV (a, rest) -> cons !a (append_impl !rest l2)
  | _ -> failwith "append"
let rec append = function
    [] -> EmptyListV
  | [x] -> x
  | a :: rest -> append_impl a (append rest)

(*
let rec reverse l =
  if nullp l then 
    EmptyListV
  else
    append_impl (reverse (cdr l)) (cons (car l) EmptyListV)
*)
let reverse x =
      let rec loop = function
          EmptyListV -> EmptyListV
	| PairV (a, rest) ->
	    append_impl (loop !rest) (cons !a EmptyListV)
	| _ -> failwith "reverse"
      in  loop x
(*
let rec list_tail ls k =
  if k = 0 then ls else list_tail (cdr ls) (k-1)
*)
let rec list_tail ls k =
  if k = 0 then
    ls
  else
    match ls with
      PairV (_, rest) -> list_tail !rest (k-1)
    | _ -> failwith "list_tail"

let list_ref ls k =
    (match (list_tail ls k) with
	PairV (a, _) -> !a
      | _ -> failwith "list_tail")


let mem pred o l =
  let rec loop = function
      EmptyListV -> BoolV false
    | PairV (a, rest) as x -> if (pred !a o) then x else loop !rest
    | _ -> failwith "Arity mismatch: mem"
  in loop l

let assoc pred o l =
  let rec loop = function
      EmptyListV -> BoolV false
    | PairV (a , rest) ->
      (match !a with 
          PairV (x, _) -> if (pred !x o) then !a else loop !rest
       | _ -> failwith "Arity mismatch: assoc")
    | _ -> failwith "Arity mismatch: assoc"
  in loop l



let display a =
    print_string (printval a); flush stdout; UnitV

let newline = function
    []  -> print_newline (); UnitV
  | _ -> failwith "Arity mismatch: newline"

let stringp = function
    StringV _ -> true
  | _ -> false

let symbolp = function
    SymbolV _ ->  true
  | _ ->  false




(* apply primitive *)
    (* (apply + 1 2 '(3 4))  : 10 *)

(* propper listであれば valtype listにする *)
let rec qqq : 'a valtype -> 'a valtype list = function
  | EmptyListV -> []
  | PairV (a, b) -> !a :: qqq !b
  | _ -> failwith "apply: not list"
(* 最後の引数の一つ前までをリストとし、最後の引数をappendする *)
let rec ppp : 'a valtype list -> 'a valtype list = function
    [] -> failwith "apply: no arguments"
  | [a] -> qqq a
  | a :: rest -> a :: (ppp rest)

(* global environment *)
let ge eval_apply =

let apply proc args =
   eval_apply proc (ppp args)


and map proc l =
  let rec impl = function
    | EmptyListV -> EmptyListV
    | PairV(x, rest) ->
        PairV (ref (eval_apply proc [!x]), ref (impl !rest))
    | _ -> failwith "not pair: map"
  in impl l

and foldl proc init l =
  let rec impl accum = function
    | EmptyListV -> accum
    | PairV(x, rest) ->
        impl (eval_apply proc [!x;accum]) !rest
    | _ -> failwith "not pair: foldl"
  in impl init l

and foldr proc init l =
  let rec impl = function
    | EmptyListV -> init
    | PairV(x, rest) ->
        let a = impl !rest in
        eval_apply proc [!x;a]
    | _ -> failwith "not pair: foldr"
  in impl  l


and makePrimV (id, f) = (id, ref (PrimV f))
in
   List.map makePrimV [
  ("+", let rec apply = function
    | [] -> 0
    | IntV a :: tl -> a + apply tl 
    | _ -> failwith "Arity mismatch: +"
  in fun args -> IntV (apply args));
  ("*", let rec apply = function
    | [] -> 1
    | IntV a :: tl -> a * apply tl 
    | _ -> failwith "Arity mismatch: +"
  in fun args -> IntV (apply args));
  ("-", minus);
  ("boolean?", function
       [x] -> BoolV (booleanp x)
     | _ -> failwith "Arity mismatch: boolean?");
  ("not", function
       [BoolV false] -> BoolV true
     | [_] -> BoolV false
     | _ -> failwith "Arity mismatch: not");
  ("eq?", function
       [x;y] -> BoolV (eqp x y)
     | _ -> failwith "Arity mismatch: eq?");
  ("eqv?", function
       [x;y] -> BoolV (eqvp x y)
     | _ -> failwith "Arity mismatch: eqv?");
  ("equal?", function
       [x;y] -> BoolV (equalp x y)
     | _ -> failwith "Arity mismatch: equal?");
  ("=", function
       [x;y] -> BoolV (eq x y)
     | _ -> failwith "Arity mismatch: =");
  ("<",  function
    | [IntV i; IntV j] -> BoolV (i < j)
    | _ -> failwith "Arity mismatch: <");
  (">",  function
    | [IntV i; IntV j] -> BoolV (i > j)
    | _ -> failwith "Arity mismatch: >");
  ("number?", function
       [x] -> BoolV (numberp x)
     | _ -> failwith "Arity mismatch: number?");
  ("pair?", function
       [x] -> BoolV (pairp x)
     | _ -> failwith "Arity mismatch: pair?");
  ("cons", function
     | [a;b] -> cons a b
     | _ -> failwith "Arity mismatch: cons");
  ("car", function
     | [x] -> car x
     | _ -> failwith "Arity mismatch: car");
  ("cdr", function
     | [x] -> cdr x
     | _ -> failwith "Arity mismatch: cdr");
  ("set-car!", function
       [p;v] -> setcar v p
     | _ -> failwith "Arity mismatch: set-car!");
  ("set-cdr!", function
       [p;v] -> setcdr v p
     | _ -> failwith "Arity mismatch: set-cdr!");
  ("caar", function
     | [x] -> car (car x)
     | _ -> failwith "Arity mismatch: caar");
  ("cadr", function
     | [x] -> car (cdr x)
     | _ -> failwith "Arity mismatch: cadr");
  ("cdar", function
       [x] -> cdr (car x)
     | _ -> failwith "Arity mismatch: cdar");
  ("cddr", function
     | [x] -> cdr (cdr x)
     | _ -> failwith "Arity mismatch: cddr");
  ("caadr", function
       [x] -> car (car (cdr x))
     | _ -> failwith "Arity mismatch: caadr");
  ("caddr", function
       [x] -> car (cdr (cdr x))
     | _ -> failwith "Arity mismatch: caddr");
  ("cdddr", function
       [x] -> cdr (cdr (cdr x))
     | _ -> failwith "Arity mismatch: cdddr");
  ("cdadr", function
       [x] -> cdr (car (cdr x))
     | _ -> failwith "Arity mismatch: cdadr");
  ("cadddr",function
       [x] -> car (cdr (cdr (cdr x)))
     | _ -> failwith "Arity mismatch: cadddr");
  ("null?", function
       [x] -> BoolV (nullp x)
     | _ -> failwith "Arity mismatch: null?");
  ("list?", function
       [x] -> BoolV (listp x)
     | _ -> failwith "Arity mismatch: list?");
  ("list", makelist);
  ("length", function
       [x] -> IntV (length 0 x)
     | _ -> failwith "Arity mismatch: length");
  ("append", append);
  ("reverse", function
     | [l] -> reverse l
     | _ -> failwith "Arity mismatch: reverse");
  ("list-tail", function
       [x; IntV k] -> list_tail x k
     | _ -> failwith "Arity mismatch: list-tail");
  ("list-ref", function
       [x; IntV k] -> list_ref x k
     | _ -> failwith "Arity mismatch: list-ref");
  ("memq", function
     | [o; l] -> mem eqp o l
     | _ -> failwith "Arity mismatch: memq");
  ("memv", function
     | [o; l] -> mem eqvp o l
     | _ -> failwith "Arity mismatch: memv");
  ("member", function
     | [o; l] -> mem equalp o l
     | _ -> failwith "Arity mismatch: assq");
  ("assq",  function
     | [o; l] -> assoc eqp o l
     | _ -> failwith "Arity mismatch: assq");
  ("assv", function
     | [o; l] -> assoc eqvp o l
     | _ -> failwith "Arity mismatch: assv");
  ("assoc", function
     | [o; l] -> assoc equalp o l
     | _ -> failwith "Arity mismatch: assoc");
  ("map", function
       [proc; l] -> map proc l
     | _ -> failwith "Arity mismatch: map");
  ("foldl", function
       [proc; init; l] -> foldl proc init l
     | _ -> failwith "Arity mismatch: foldl");
  ("foldr", function
       [proc; init; l] -> foldr proc init l
     | _ -> failwith "Arity mismatch: foldr");

  ("write", function 
       [a] -> display a
     | _ -> failwith "Arity mismatch: write");
  ("display", function 
       [a] -> display a
     | _ -> failwith "Arity mismatch: display");
  ("newline", newline);
  ("read", function
       [] -> read ()
     | _ -> failwith "Arity mismatch: read");
  ("string?", function
       [x] -> BoolV (stringp x)
     | _ -> failwith "Arity mismatch: string?");
  ("symbol?", function
       [x] -> BoolV (symbolp x)
     | _ -> failwith "Arity mismatch: symbol?");
  ("apply", function
       (x :: y) -> apply x y
     | _ -> failwith "Arity mismatch: apply");
  ("force", function
       [x] -> apply x [EmptyListV]
     | _ -> failwith "Arity mismatch: force");
  ]

