
(*
 sicp 4.1.7
*)

open Parser

open Value

(*
  2014/3/1
    -rectypes をつけるとコンパイルできる
 *)


(*
type env = (id * valtype ref) list
and
valtype =
    IntV of int
  | BoolV of bool
  | CharV of char
  | StringV of string
  | SymbolV of id
  | ProcV of id list * (id * proctype) list * proctype * env
  | PrimV of (valtype list -> valtype) 
  | PairV of valtype ref * valtype ref
  | EmptyListV
(*  | ProperListV of valtype list *)
(* list?の判定が定数時間で完了する。空リストを認めないとpair?の判定が簡単になる。ただしcdrの操作に注意が必要 *)
  | VectorV of valtype array
  | UnitV
  | UnboundV
(* analyzeExpが返す型 *)
and proctype = env -> valtype



(* interpreter *)
let rec lookup a : (env -> valtype ref) = function
    [] -> failwith ("runtime: var " ^ a ^ " not exist")
  | (x, v) :: rest -> if a = x then v else lookup a rest



(* applyのargsをrefに変換して環境に追加する *)
let rec extend env ids args  =
    match (ids, args) with
      [], [] -> env
    | id :: b, ex :: d ->
        (id, ref ex) :: extend env b d
    | _, _ -> failwith "parameter unmatch"

 *)

type 'a proctype = 'a env -> 'a valtype

(*
let evalSelf = function
  | IntExp x -> IntV x
  | BoolExp x -> BoolV x
  | CharExp x -> CharV x
  | StringExp x -> StringV x
  | UnitExp   -> UnitV
  | _ -> failwith "eval_self"
 *)
let rec evalSelf = function
  | Syntax.Int x -> IntV x
  | Syntax.Bool x -> BoolV x
  | Syntax.Char x -> CharV x
  | Syntax.String x -> StringV x


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

let rec analyzeExp  : exp -> 'a proctype = function
(*
  | IntExp _  | BoolExp _  | CharExp _ | StringExp _ as x ->
      let result = evalSelf x in (fun _ -> result)
 *)
  | SelfEvalExp sexp -> let result = evalSelf sexp in fun _ -> result
  | UnitExp -> fun _ -> UnitV
  | VarExp x -> fun env -> !(lookup x env)
  | QuoteExp x -> let result = evalQuote x in fun _ -> result
  | IfExp (c, a, b) ->
      let pred  = analyzeExp c
      and conseq = analyzeExp a
      and alt = analyzeExp b in
      (fun env ->
       (match pred env with
	| BoolV false -> alt | _ -> conseq) env)

  | AndExp ls ->
     let args = List.map analyzeExp ls in
     fun env ->
      let rec loop result = function
        | [] -> result
        | p :: rest ->
            (match result with
	       BoolV false -> BoolV false | _ -> loop (p env) rest)
      in loop (BoolV true) args
 
  | OrExp ls ->
     let args = List.map analyzeExp ls in
     fun env ->
      let rec loop result = function
          [] -> result
        | p :: rest ->
            (match result with
	       BoolV false -> loop (p env) rest | e -> e)
      in loop (BoolV false) args

  | LambdaExp (ids, (defs, exp)) ->
     let proc = analyzeExp exp
     and dd = List.map (fun (id, ex) -> id, analyzeExp ex) defs in
     fun env ->
       ProcV (ids, dd, proc, env)

  | ApplyExp (exp, args) ->
     let proc = analyzeExp exp
     and a = List.map analyzeExp args in
     fun env ->
       let aa = List.map (fun p -> p env) a in 
       eval_apply (proc env) aa
(*
  | LetExp (binds, (defs, exp)) ->
      let a = extendlet env binds in
      let b = extendletrec a defs in
      evalExp b exp
 *)
  | NamedLetExp (id, binds, body) ->
      let (ids, args) = List.split binds
      and pid = analyzeExp (VarExp id) in
      let arr = List.map analyzeExp args
      and fn = LambdaExp (ids, body) in
      let pfn = [id, analyzeExp fn] in
      fun env ->
        let a = extendletrec env pfn in
        (* todo: pid a ではなく lookup id a でいいかもしれない  *)
	eval_apply (pid a) (List.map (fun p -> p env) arr)

  | LetrecExp (binds, (defs, exp)) ->
     let e = analyzeExp exp
     and bb = List.map (fun (id, ex) -> (id, analyzeExp ex)) binds 
     and dd = List.map (fun (id, ex) -> (id, analyzeExp ex)) defs in
     fun env ->
       let a = extendletrec env bb in
       let b = extendletrec a dd in
       e b

  | CondClauseExp x ->
      (match x with
	FUN (cond, conseq, alt) ->
	 let pcond = analyzeExp cond
	 and pcon = analyzeExp conseq
	 and palt = analyzeExp alt in
	 fun env -> (match pcond env with
		       BoolV false -> palt env
		     | e -> eval_apply (pcon env) [e])
      | VAL (cond, alt) ->
	 let pcond = analyzeExp cond
	 and palt = analyzeExp alt in
	 fun env -> (match pcond env with
		      | BoolV false -> palt env | e -> e)
      )
 
  | SetExp (id, exp) ->
     let e = analyzeExp exp in
     fun env ->
       let a = lookup id env in
       a := (e env); UnitV
(*
  | BeginExp exps ->
     let a = List.map analyzeExp exps in
     fun env ->
      let result = ref UnitV in
      List.iter (fun x -> result := x env) a;
      !result
 *)
  | BeginExp exps ->
     analyze_seq exps

and analyze_seq exps =
(*
  let sequentially proc1 proc2 =
    fun env -> proc1 env; proc2 env in
*)
  let sequentially proc1 proc2 env = proc1 env; proc2 env in
  let rec loop proc = (function
	[] -> proc (* proc自体が proctypeのため,funで包まなくてよい  *)
      | a :: b -> loop (sequentially proc a) b) in
  let procs = List.map analyzeExp exps in
  (match procs with
     [] -> failwith "empty seq"
   | a :: b -> loop a b)

and eval_apply proc args =
  (match proc with
     ProcV (ids, pdefs, pexp, en) -> (* procには定義リストもある  *)
      let newenv = extend en ids args in 
      let newnewenv = extendletrec newenv pdefs in
      pexp newnewenv
   | PrimV closure ->
      closure args
   | _  -> failwith "not proc"
  )

and extendletrec env binds  =
  let rec ext = function
      [] -> env
    | (id, _) :: rest -> (id, ref UnboundV) :: ext rest in
  let newenv = ext binds in
  let rec loop e = function
      [] -> ()
    | (_, pexp) :: rest  ->
	let (_, v) :: r = e in
        v := pexp newenv; loop r rest
  in
  loop newenv binds;
  newenv


(*
let makePrimV (id, f) = (id, ref (PrimV f))
 *)
  


let pplist =
  let rec pprest = function
      [] -> ""
    | a :: b ->  " " ^ a ^ pprest b in
  function
      [] -> "()"
    | x :: rest -> "(" ^  x ^ pprest rest ^ ")"

let rec printval = function
    IntV i -> string_of_int i
  | BoolV x ->  (if x then "#t" else "#f")
(*  | CharV x -> string_of_char x *)
  | SymbolV x ->  (x)
  | StringV x ->  x (* "\"" ^ x ^ "\"" *)
  | ProcV (args, _,_, _) ->
       "#proc:" ^ (pplist args)
  | PrimV _ ->  "primitive"
  | PairV (a, b) ->  "(" ^ printval !a ^ pppair !b ^ ")"
  | EmptyListV ->  "()"
  | UnboundV ->  "*unbound*"
  | UnitV -> "#void"

and pppair = function(* PairVの第2要素 *)
    EmptyListV -> ""
  | PairV (a, b) ->  " " ^ printval !a ^ pppair !b
  | arg -> " . " ^ printval arg







let minus (ls :'a valtype list) =
  let rec apply = function
    | [IntV i; IntV j] -> i - j
    | _ -> failwith "Arity mismatch: -"
  in IntV (apply ls)

let eq x y =
  match (x,y) with
    | IntV i, IntV j -> i = j 
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

let rec length r = function
    EmptyListV -> r
  | PairV (_, rest) -> length (r + 1) !rest
  | _ -> failwith "Arity mismatch: length"

let assoc pred o l =
  let rec loop = function
      EmptyListV -> BoolV false
    | PairV (a , rest) ->
      (match !a with 
          PairV (x, _) -> if (pred !x o) then !a else loop !rest
       | _ -> failwith "Arity mismatch: assoc")
    | _ -> failwith "Arity mismatch: assoc"
  in loop l

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


let display a =
    print_string (printval a); flush stdout; UnitV

let newline = function
    []  -> print_newline (); UnitV
  | _ -> failwith "Arity mismatch: newline"


let read () =
  let buf = Lexing.from_channel stdin in
  (* let buf = Lexing.from_string (input_line  stdin) in *) (* 本来 1s式をよむ。次の readで残りのs  *)
  let sexp = Sparser.sexpdata Lexer.main buf in
     evalQuote sexp



(* propper listであれば valtype listにする *)
let rec qqq : 'a valtype -> 'a valtype list = function
  | EmptyListV -> []
  | PairV (a, b) -> !a :: qqq !b
  | _ -> failwith "apply: not list"
(* 最後の引数が,listであれば、最後の一つ前までをリストとして、最後の引数をappendする *)
let rec ppp : 'a valtype list -> 'a valtype list = function
    [] -> []
  | [a] -> qqq a
  | a :: rest -> a :: (ppp rest)


let apply (proc : 'a valtype) (args : 'a valtype list) =
   eval_apply proc (ppp args)


let primis = 
  List.map (fun (id, f) -> (id, ref (PrimV f))) [
  ("+", let rec apply = function
    | [IntV i] -> i
    | IntV a :: tl -> a + apply tl 
    | _ -> failwith "Arity mismatch: +"
	in fun args -> IntV (apply args));
  ("*", let rec apply = function
    | [IntV i] -> i
    | IntV a :: tl -> a * apply tl 
    | _ -> failwith "Arity mismatch: +"
	in fun args -> IntV (apply args));
  ("-", minus);
  ("=", function
       [x;y] -> BoolV (eq x y)
     | _ -> failwith "Arity mismatch: equal?");
  ("<", function
    | [IntV i; IntV j] -> BoolV (i < j)
    | _ -> failwith "Arity mismatch: =");
  (">", function
    | [IntV i; IntV j] -> BoolV (i > j)
    | _ -> failwith "Arity mismatch: =");
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
  ("cons", function
     | [x;y] -> cons x y
     | _ -> failwith "Arity mismatch: cons");
  ("car", function
     | [x] -> car x
     | _ -> failwith "Arity mismatch: car");
  ("cdr", function
     | [x] -> cdr x
     | _ -> failwith "Arity mismatch: cdr");
  ("caar", function
     | [x] -> car (car x) 
     | _ -> failwith "Arity mismatch: caar");
  ("cadr", function
     | [x] -> car (cdr x) 
     | _ -> failwith "Arity mismatch: cadr");
  ("cddr", function
     | [x] -> cdr (cdr x) 
     | _ -> failwith "Arity mismatch: cddr");
  ("caadr", function
     | [x] -> car (car (cdr x)) 
     | _ -> failwith "Arity mismatch: caadr");
  ("caddr", function
     | [x] -> car (cdr (cdr x)) 
     | _ -> failwith "Arity mismatch: caddr");
  ("cdadr", function
     | [x] -> cdr (car (cdr x)) 
     | _ -> failwith "Arity mismatch: caddr");
  ("cdddr", function
     | [x] -> cdr (cdr (cdr x)) 
     | _ -> failwith "Arity mismatch: caddr");
  ("cadddr", function
     | [x] -> car (cdr (cdr (cdr x))) 
     | _ -> failwith "Arity mismatch: caddr");
  ("set-car!", function
       [p;v] -> setcar v p
     | _ -> failwith "Arity mismatch: set-car!");
  ("set-cdr!", function
       [p;v] -> setcdr v p
     | _ -> failwith "Arity mismatch: set-cdr!");

  ("null?", function
       [x] -> BoolV (nullp x)
     | _ -> failwith "Arity mismatch: null?");
  ("list", let rec makelist = function
       [] -> EmptyListV
     | a :: rest -> cons a (makelist rest) in
	   fun l -> makelist l);
  ("length", function
       [x] -> IntV (length 0 x)
     | _ -> failwith "Arity mismatch: length");
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
       [proc; l] ->
       let rec map = function
	 | EmptyListV -> EmptyListV
	 | PairV(x, rest) ->
            PairV (ref (eval_apply proc [!x]), ref (map !rest))
	 | _ -> failwith "not pair: map"
       in map l
     | _ -> failwith "Arity mismatch: map");
  ("number?", function
     [IntV _] -> BoolV true
   | [_] -> BoolV false
   | _ -> failwith "Arity mismatch: number");
  ("string?", function
     [StringV _] -> BoolV true
   | [_] -> BoolV false
   | _ -> failwith "Arity mismatch: number");
  ("symbol?", function
     [SymbolV _] -> BoolV true
   | [_] -> BoolV false
   | _ -> failwith "Arity mismatch: number");
  ("pair?", function
     [PairV _] -> BoolV true
   | [_] -> BoolV false
   | _ -> failwith "Arity mismatch: number");
  ("apply", function
       (x :: y) -> apply x y
     | _ -> failwith "Arity mismatch: apply2");
  ("force", function
       [x] -> apply x []
     | _ -> failwith "Arity mismatch: force");


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
]


let parse s =
  parseExp (Sparser.sexpdata Lexer.main (Lexing.from_string s))


let analyze s =
  analyzeExp (parse s)

let evallex sexps =
  let (defs, exp) = Parser.parseBody sexps in
  let dd = List.map (fun (id, ex) -> (id, analyzeExp ex)) defs in
  let envv = extendletrec primis dd in
    (analyzeExp exp) envv

let eval s =
  let x = Sparser.toplevel Lexer.main (Lexing.from_string s) in
   (evallex x)

let lex_from name =
  let f = open_in name in
  try
    let s = Sparser.toplevel Lexer.main (Lexing.from_channel f) in
    close_in f; s
  with Failure msg -> close_in f; raise (Failure msg)


let interpret name =
   printval (evallex (lex_from name))


let _ =
 let fn = ref [] in
   Arg.parse [] (fun s -> fn := s :: !fn) "";
   if List.length !fn > 0 then
     print_endline (interpret (List.hd !fn))
   else ()
