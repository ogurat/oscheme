(* oschme scheme.ml *)

open Parser
open Valtype



(* primitives *)


let rec printval = function
    IntV i -> string_of_int i
  | BoolV x ->  (if x then "#t" else "#f")
  | CharV x -> String.make 1 x
  | SymbolV x -> (x)
  | StringV x -> x (* "\"" ^ x ^ "\"" *)
  | VectorV x ->
     let rec loop n = 
       if n = Array.length x then
         ""
       else
         " " ^ printval x.(n) ^ loop (n + 1) in
     if Array.length x > 0 then
       "#(" ^ printval x.(0) ^ loop 1 ^ ")"
     else
       "#()"
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
  | PairV (a, b) ->
     (match !a with
     | SymbolV "quasiquote" -> ppq "`" !a !b
     | SymbolV "unquote" -> ppq "," !a !b
     | SymbolV "unquote-splicing" -> ppq ",@" !a !b
     | SymbolV "quote" -> ppq "'" !a !b
     | _ ->  "(" ^ printval !a ^ pppair !b ^ ")"
     )
  | EmptyListV -> "()"
  | UnboundV ->  "*unbound*"
  | UnitV -> "#void"

and pppair = function (* PairVの第2要素 *)
    EmptyListV -> ""
  | PairV (a, b) ->  " " ^ printval !a ^ pppair !b
  | arg -> " . " ^ printval arg
and ppq q x = function (* quote *)
  | PairV (a, b) when !b = EmptyListV -> q ^ printval !a
  | y ->
     let a = (match y with 
              | EmptyListV -> ""
              | PairV (a, b) ->  " " ^ printval !a ^ pppair !b
              | arg -> " . " ^ printval arg
             ) in
     "(" ^ printval x ^ a ^ ")"
     



(* schemeの値の真偽を評価する bool値のfalseだけが偽と評価されその他は真と評価される *)
let truep = function
  BoolV false -> false
| _           -> true



let read () =
  let buf = Lexing.from_channel stdin in
  (* let buf = Lexing.from_string (input_line  stdin) in *) (* 本来 1s式をよむ。次の readで残りのs  *)
  let sexp = Sparser.sexpdata Lexer.main buf in
     evalQuote sexp

let chareqp a b =
  match (a, b) with
    (CharV x, CharV y) -> x = y
  | _  -> false

let symboleqp a b =
  match (a, b) with
    (SymbolV x, SymbolV y) -> x = y
  | _  -> false

let eqp x y =
  match (x, y) with
  | (BoolV true, BoolV true) | (BoolV false, BoolV false) ->  true
  | (SymbolV _, SymbolV _) -> symboleqp x y
  | (EmptyListV, EmptyListV) -> true
  | PairV _ , PairV _  -> x == y
  | StringV a, StringV b -> x == y
  | VectorV a, VectorV b -> x == y
(* also bytevector, record  *)
  | _ ->  false

let eqvp x y =
  match (x, y) with
  | a,b when (eqp a b) -> true
  | (IntV x, IntV y) -> x = y
  | (CharV _, CharV _) -> chareqp x y
  | _ ->  false

let rec equalp x y =
  match (x, y) with
  | a,b when (eqvp a b) ->  true
  | PairV (a, b), PairV (c, d) -> (equalp !a !c) && (equalp !b !d)
  | StringV x, StringV y -> x = y
  | VectorV x, VectorV y -> x = y (* todo: recursive *)
  | _ ->  false



let add_ ls =
  let c =List.fold_left (fun x y ->
                         match y with
                           IntV y -> x + y
                          | _ -> failwith "Arity mismatch: +")
                        0 ls
  in IntV c

let add ls =
  let rec apply result = function
    | [] -> result
    | IntV a :: tl -> apply (a + result) tl 
    | _ -> failwith "Arity mismatch: +"
  in IntV (apply 0 ls);;

let multi ls = 
  let rec apply result = function
    | [] -> result
    | IntV a :: tl -> apply (a * result) tl 
    | _ -> failwith "Arity mismatch: *"
  in IntV (apply 1 ls)

let minus ls =
  let rec apply = function
    | [IntV i; IntV j] -> i - j
    | _ -> failwith "Arity mismatch: -"
  in IntV (apply ls)

let comp op ls =
  let rec loop = function
      [IntV a; IntV b] -> op a b
    | IntV a :: rest ->
       (match rest with
          IntV b :: _ ->  op a b && loop rest
        | _ ->  failwith "comp"
       )
    | _ -> failwith "comp"     
  in BoolV (loop ls)

let booleanp = function
    BoolV _ ->  true
  | _ -> false

let not v =
    if (truep v) then false else true

let eq x y =
  match (x,y) with
    | IntV i, IntV j -> i = j 
    | _ -> false

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
  | x -> failwith ("Arity mismatch: car not pair" ^ (printval x))

let cdr = function 
    PairV (_, x) -> !x
  | x -> failwith ("Arity mismatch: cdr not pair " ^ (printval x))

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
  | x -> length (r + 1) (cdr x)

  
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
  | x -> cons (car x) (append_impl (cdr x) l2)
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
    list_tail (cdr ls) (k-1)

let list_ref ls k =
  (car (list_tail ls k))



let mem pred o l =
  let rec loop = function
      EmptyListV -> BoolV false
    | x -> if (pred (car x) o) then x else loop (cdr x)
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
let rec string_append  = function
    [] -> ""
  | StringV x :: rest -> x ^ string_append rest
  | _ -> failwith "Arity mismatch: string-append"

let symbolp = function
    SymbolV _ ->  true
  | _ ->  false

let charp = function
    CharV _ ->  true
  | _ ->  false

let vectorp = function
    VectorV _ ->  true
  | _ ->  false
(*
let vec_length = function
    VectorV x -> Array.length x
  | _ -> failwith "Arity mismatch: vector-length not vector"
 *)
let vec_to_list = function
    VectorV x -> Array.fold_right (fun x y -> PairV (ref x, ref y)) x EmptyListV
  | _ -> failwith "Arity mismatch: vector->list not vector"

let makevector args =
   VectorV (Array.of_list args)


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
let ge apply =

let apply proc args =
   apply proc (ppp args)


and map1 proc =
  let rec impl = function
    | EmptyListV -> EmptyListV
    | x -> cons (apply proc [car x]) (impl (cdr x))
  in impl

and map2 proc =
  let rec impl x y =
    (match x, y with
    | EmptyListV, _ | _, EmptyListV -> EmptyListV
    | x, y ->
        cons (apply proc [car x; car y]) (impl (cdr x) (cdr y))
    )
  in impl
(*
and map3 proc l1 l2 l3 =
  let rec impl x1 x2 x3 =
    (match x1, x2, x3 with
    | EmptyListV, _,_ | _, EmptyListV,_ | _,_,EmptyListV -> EmptyListV
    | PairV(x1, rest1), PairV (x2, rest2), PairV(x3, rest3) ->
        PairV (ref (apply proc [!x1; !x2; !x3]), ref (impl !rest1 !rest2 !rest3))
    | _ -> failwith "not pair: map"
    )
  in impl l1 l2 l3
 *)

and map proc =
  let rec impl xs =
    if List.exists (function  EmptyListV -> true | _ -> false) xs then
      EmptyListV
    else
      let a = List.map car xs
      and b = List.map cdr xs in
      cons (apply proc a) (impl b) 
  in impl

and foreach proc =
  let rec impl = function
    | EmptyListV -> UnitV
    | x -> apply proc [car x]; impl (cdr x)
  in impl

and foldleft proc =
  let rec impl accum = function
    | EmptyListV -> accum
    | x -> impl (apply proc [accum; (car x)]) (cdr x)
  in impl

and fold proc =
  let rec impl accum = function
    | EmptyListV -> accum
    | x -> impl (apply proc [(car x); accum]) (cdr x)
  in impl

and foldright proc init =
  let rec impl = function
    | EmptyListV -> init
    | x -> apply proc [(car x); impl (cdr x)]
  in impl


and makePrimV (id, f) = (id, ref (PrimV f))
in
   List.map makePrimV [
  ("+", add);
  ("*", multi);
  ("-", minus);
  ("abs", function
     [IntV x] -> IntV (abs x)
   | _ -> failwith "Arity mismatch: abs");
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


  ("number?", function
       [x] -> BoolV (numberp x)
     | _ -> failwith "Arity mismatch: number?");
  ("=", comp (=));
  ("<", comp (<));
  (">", comp (>));
  ("<=", comp (<=));
  (">=", comp (>=));
  ("zero?", function
     [IntV x] -> BoolV (x = 0)
   | _ -> failwith "Arity mismatch: zero?");
  ("positive?", function
     [IntV x] -> BoolV (x > 0)
   | _ -> failwith "Arity mismatch: positive?");

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
  ("caaar", function
       [x] -> car (car (car x))
     | _ -> failwith "Arity mismatch: caaar");
  ("caadr", function
       [x] -> car (car (cdr x))
     | _ -> failwith "Arity mismatch: caadr");
  ("cadar", function
       [x] -> car (cdr (car x))
     | _ -> failwith "Arity mismatch: cadar");
  ("caddr", function
       [x] -> car (cdr (cdr x))
     | _ -> failwith "Arity mismatch: caddr");
  ("cdaar", function
       [x] -> cdr (car (car x))
     | _ -> failwith "Arity mismatch: cdaar");
  ("cdadr", function
       [x] -> cdr (car (cdr x))
     | _ -> failwith "Arity mismatch: cdadr");
  ("cddar", function
       [x] -> cdr (cdr (car x))
     | _ -> failwith "Arity mismatch: cddar");
  ("cdddr", function
       [x] -> cdr (cdr (cdr x))
     | _ -> failwith "Arity mismatch: cdddr");
  ("caaaar",function
       [x] -> car (car (car (car x)))
     | _ -> failwith "Arity mismatch: caaaar");
  ("caaadr",function
       [x] -> car (car (car (cdr x)))
     | _ -> failwith "Arity mismatch: caaadr");
  ("caadar",function
       [x] -> car (car (cdr (car x)))
     | _ -> failwith "Arity mismatch: caadar");
  ("caaddr",function
       [x] -> car (car (cdr (cdr x)))
     | _ -> failwith "Arity mismatch: caaddr");
  ("cadaar",function
       [x] -> car (cdr (car (car x)))
     | _ -> failwith "Arity mismatch: cadaar");
  ("cadadr",function
       [x] -> car (cdr (car (cdr x)))
     | _ -> failwith "Arity mismatch: cadadr");
  ("caddar",function
       [x] -> car (cdr (cdr (car x)))
     | _ -> failwith "Arity mismatch: cadaar");
  ("cadddr",function
       [x] -> car (cdr (cdr (cdr x)))
     | _ -> failwith "Arity mismatch: cadddr");
  ("cdaaar",function
       [x] -> cdr (car (car (car x)))
     | _ -> failwith "Arity mismatch: cdaaar");
  ("cdaadr",function
       [x] -> cdr (car (car (cdr x)))
     | _ -> failwith "Arity mismatch: caaaar");
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
       [proc; l] -> map1 proc l
     | [proc; l1; l2] -> map2 proc l1 l2
(*
     | [proc; l1; l2; l3] -> map3 proc l1 l2 l3
 *)
     | proc :: ls -> map proc ls
     | _ -> failwith "Arity mismatch: map");
  ("for-each", function
       [proc; l] -> foreach proc l
     | _ -> failwith "Arity mismatch: for-each");
  ("fold-left", function
       [proc; init; l] -> foldleft proc init l
     | _ -> failwith "Arity mismatch: foldl");
  ("fold", function
       [proc; init; l] -> fold proc init l
     | _ -> failwith "Arity mismatch: foldl");
  ("fold-right", function
       [proc; init; l] -> foldright proc init l
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
  ("eof-object?", function
     [v] -> BoolV false
   | _ -> failwith "Arity mismatch: eof-object?");

  ("string?", function
       [x] -> BoolV (stringp x)
     | _ -> failwith "Arity mismatch: string?");
  ("string-length", function
     [StringV x] -> IntV (String.length x)
   | _ -> failwith "Arity mismatch: string-length");
  ("string-ref", function
     [StringV s; IntV i] -> CharV (s.[i])
   | _ -> failwith "Arity mismatch: string-ref");
  ("string-append", fun x ->
                    StringV (string_append x));
  ("string->symbol", function
     [StringV x] -> SymbolV x
   | _ -> failwith "Arity mismatch: symbol->sting");

  ("number->string", function
     [IntV x] -> StringV (string_of_int x)
   | _ -> failwith "Arity mismatch: number->sting");

  ("char?", function
     [x] -> BoolV (charp x)
   | _ -> failwith "Arity mismatch: char?");
  ("char=?", function
     [x1; x2] -> BoolV (chareqp x1 x2)
   | _ -> failwith "Arity mismatch: char=?");

  ("vector?", function
     [x] -> BoolV (vectorp x)
   | _ -> failwith  "Arity mismatch: vector?");
  ("vector-length", function
     [VectorV x] -> IntV (Array.length x)
   | _ -> failwith  "Arity mismatch: vector-length");
  ("vector", makevector);
  ("make-vector", function
     [IntV k; x] -> VectorV (Array.make k x)
   | _ ->  failwith  "Arity mismatch: make-vector");
  ("vector-ref", function
     [VectorV x; IntV i] -> x.(i)
   | _ ->  failwith "Arity mismatch: vector-ref");
  ("vector->list", function
     [x] -> vec_to_list x
   | _  -> failwith "Arity mismatch: vector->list");

  ("symbol?", function
     [x] -> BoolV (symbolp x)
   | _ -> failwith "Arity mismatch: symbol?");
  ("symbol=?", function
     [x; y] -> BoolV (symboleqp x y)
   | _ -> failwith "Arity mismatch: symbol=?");
  ("symbol->string", function
     [SymbolV x] -> StringV x
   | _ -> failwith "Arity mismatch: symbol->sting");

  ("apply", function
       (x :: y) -> apply x y
     | _ -> failwith "Arity mismatch: apply");
  ("force", function
       [x] -> apply x [EmptyListV]
     | _ -> failwith "Arity mismatch: force");
  ]
