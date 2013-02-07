(* oschme scheme.ml *)

open Eval


(* primitives *)

let print_env env =
  List.iter 
    (fun (id, v) ->
      (match !v with
        IntV x  -> print_string (id ^ ":" ^ string_of_int x)
      | ProcV _ -> print_string id;
      | _ -> ()  );
      print_string ";"
    )
    env

let pplist =
  let rec pprest = function
      [] -> ""
    | a :: b ->  " " ^ a ^ pprest b in
  function
      [] -> ""
    | x :: rest -> "(" ^  x ^ pprest rest ^ ")"


let rec printval = function
    IntV i -> string_of_int i
  | BoolV x ->  (if x then "#t" else "#f")
(*  | CharV x -> string_of_char x *)
  | SymbolV x ->  (x)
  | StringV x ->  x (* "\"" ^ x ^ "\"" *)
  | ProcV (args, (deflist, _), env) ->
       "#proc:" ^ (pplist args)
  | PrimV _ ->  "primitive"
  | PairV (a, b) ->  "(" ^ printval !a ^ pppair !b ^ ")"
  | EmptyListV ->  "()"
  | UnboundV ->  "*unbound*"
  | UnitV -> ""

and pppair = function(* PairVの第2要素 *)
    EmptyListV -> ""
  | PairV (a, b) ->  " " ^ printval !a ^ pppair !b
  | arg -> " . " ^ printval arg



(* evalQuoteと同じ
let rec stoobject = function
    Syntax.Id s -> SymbolV s
  | Syntax.Bool x -> BoolV x
  | Syntax.Char x -> CharV x
  | Syntax.String x -> StringV x
  | Syntax.Int x -> IntV x
  | Syntax.List x -> 
      let rec loop = function
        [] -> EmptyListV
      | a :: rest -> PairV (ref (stoobject a), ref (loop rest)) in
      loop x
*) 

let stolistt s = []

let read () =
  let buf = Lexing.from_channel stdin in
  (* let buf = Lexing.from_string (input_line  stdin) in *) (* 本来 1s式をよむ。次の readで残りのs  *)
  let sexp = Sparser.sexpdata Lexer.main buf in
     Eval.evalQuote sexp

let chareq a b = BoolV (a = b)

let eqp x y =
  match (x, y) with
  | (BoolV true, BoolV true) 
  | (BoolV false, BoolV false) ->  true
  | (SymbolV s, SymbolV s2) when s = s2 ->  true
  | (EmptyListV, EmptyListV) ->  true
  | StringV a, StringV b when a == b ->  true
  | VectorV a, VectorV b when a == b ->  true
  | _ ->  false

let eqvp x y =
  match (x, y) with
  | a,b when (eqp a b) -> true
  | (IntV x, IntV y) when x = y -> true
  | (CharV c, CharV d) ->  c = d
  | _ ->  false

let rec equalp x y =
  match (x, y) with
  | PairV (a, b), PairV (c, d) -> (equalp !a !c) && (equalp !b !d)
  | VectorV a, VectorV b -> false
  | a,b when (eqvp a b) ->  true
  | _ ->  false



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

let minus (ls :valtype list) =
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


let lt (ls : valtype list) =
  let rec apply = function
    | [IntV i; IntV j] -> i < j 
    | _ -> failwith "Arity mismatch: ="
  in BoolV (apply ls)

let gt (ls : valtype list) =
  let rec apply = function
    | [IntV i; IntV j] -> i > j 
    | _ -> failwith "Arity mismatch: ="
  in BoolV (apply ls)

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


let mapimpl proc l =
  let rec map = function
    | EmptyListV -> EmptyListV
    | PairV(x, rest) ->
        PairV (ref (eval_apply proc [!x]), ref (map !rest))
    | _ -> failwith "not pair: map"
  in map l

let foldlimpl proc init l =
  let rec fold accum = function
    | EmptyListV -> accum
    | PairV(x, rest) ->
        fold (eval_apply proc [!x;accum]) !rest
    | _ -> failwith "not pair: foldl"
  in fold init l

let foldrimpl proc init l =
  let rec fold = function
    | EmptyListV -> init
    | PairV(x, rest) ->
        let a = fold !rest in
        eval_apply proc [!x;a]
    | _ -> failwith "not pair: foldr"
  in fold  l

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
    (* (apply + 1 2 '(3))  : 6 *)

(* propper listであれば valtype listにする *)
let rec qqq = function
  | EmptyListV -> []
  | PairV (a, b) -> !a :: qqq !b
  | _ -> failwith "listでない"
let rec ppp : valtype list -> valtype list = function
    [a] -> if  (listp a) then qqq a else failwith "listでない" 
  | a :: rest -> a :: (ppp rest)

let apply (proc : valtype) (args : valtype list) =
  let rec evalExplist env a ids args  : env = (* 評価後順序が反転する *)
    match (ids, args) with
      [], [] -> a
    | id :: b, ex :: d ->
        evalExplist env ((id, ref ex) :: a) b d
    | _, _ -> failwith "parameter unmatch" in
    let newargs = ppp args in
  (match proc with
    ProcV (ids, (defs, ex), en) -> (* procには定義リストもある  *)
      let newenv = evalExplist [] en ids newargs in
      let newnewenv = extendletrec newenv defs in
      evalExp newnewenv ex 
  | PrimV closure ->
      closure newargs
  | _ -> failwith "apply:not proc");;

let apply2 (proc : valtype) (args : valtype list) =
   let newargs = ppp args in
   eval_apply proc newargs



let primis = 
  let a = [
  ("+", add);
  ("*", multi);
  ("-", minus);
  ("boolean?", function
       [x] -> BoolV (booleanp x)
     | _ -> failwith "Arity mismatch: boolean?");
  ("not",  function
       [x] -> BoolV (not x)
     | _ -> failwith "Arity mismatch: not");
  ("eq?", function
       [x;y] -> BoolV (eqp x y)
     | _ -> failwith "Arity mismatch: eq?");
  ("eqv?",  function
       [x;y] -> BoolV (eqvp x y)
     | _ -> failwith "Arity mismatch: eqv?");
  ("equal?", function
       [x;y] -> BoolV (equalp x y)
     | _ -> failwith "Arity mismatch: equal?");
  ("=", function
       [x;y] -> BoolV (eq x y)
     | _ -> failwith "Arity mismatch: equal?");
  ("<", lt);
  (">", gt);
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
  ("cddr",  function
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
       [proc;l] -> mapimpl proc l
     | _ -> failwith "Arity mismatch: map");
  ("foldl", function
       [proc;init;l] -> foldlimpl proc init l
     | _ -> failwith "Arity mismatch: foldl");
  ("foldr", function
       [proc;init;l] -> foldrimpl proc init l
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
  ("apply",  (fun (x :: y) -> apply x y));
  ("apply2", (fun (x :: y) -> apply2 x y));

  ] in
  List.map (fun (id, f) -> (id, ref (PrimV f))) a



(* 環境を先に評価すると定義の順序が問題となる *)
(* 環境をlookupしたときに評価すべきか *)
let evallex sexps =
  (* let sexps = Sparser.toplevel Lexer.main lexbuf in *)
  let (defs, exp) = Parser.parseBody sexps in
  let envv = extendletrec primis defs in
    (* let envv = List.map (fun (id, exp) -> (id, ref (evalExp [] exp))) deflist in *)
    evalExp envv exp;;

let eval s =
  evallex (Sparser.toplevel Lexer.main (Lexing.from_string s))

  
let openandlex name =
  let f = open_in name in
  try
    let s = Sparser.toplevel Lexer.main (Lexing.from_channel f) in
    close_in f; s
  with Failure msg -> close_in f; raise (Failure msg)

let interprete name =
  print_string (printval (evallex (openandlex name)))

let parseExp s =
  Parser.parseExp (Sparser.sexpdata Lexer.main (Lexing.from_string s))

let _ =
  let fn = ref [] in
  Arg.parse [] (fun s -> fn := s :: !fn) "";
  interprete (List.hd !fn)

(*
ocaml sparser.cmo  parser.cmo lexer.cmo eval.cmo scheme.cmo
*)
