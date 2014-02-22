

open Parser



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
  | (x, v) :: rest -> if a = x then v else lookup a rest;;



(* applyのargsをrefに変換して環境に追加する *)
let rec extend env ids args : env =
    match (ids, args) with
      [], [] -> env
    | id :: b, ex :: d ->
        (id, ref ex) :: extend env b d
    | _, _ -> failwith "parameter unmatch"


(*
type ccc = FunProcV of id list * (env -> valtype) * env
 *)
let eval_self = function
  | IntExp x -> IntV x
  | BoolExp x -> BoolV x
  | CharExp x -> CharV x
  | StringExp x -> StringV x
  | _ -> failwith "eval_self"

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

let rec analyzeExp = function

  | IntExp _  | BoolExp _  | CharExp _ | StringExp _ as x  ->
		    let result = eval_self x in
		    (fun _ -> result)
  | VarExp x -> fun env -> !(lookup x env)
  | QuoteExp x -> let result = evalQuote x in fun _ -> result
  | UnitExp -> fun env -> UnitV
  | IfExp (c, a, b) ->
      let pred  = analyzeExp c
	and conseq = analyzeExp a
	and alt = analyzeExp b in
      (fun env ->
       (match pred env with
	| BoolV false -> alt env
	| _ -> conseq env))

  | AndExp ls ->
     let args = List.map analyzeExp ls in
     fun env ->
      let rec loop = function
        | [] -> BoolV true
	| [a] -> a env
        | a :: rest ->
            (match a env with
	       BoolV false -> BoolV false
	     | _ -> loop rest)
      in loop args
 
  | OrExp ls ->
     let args = List.map analyzeExp ls in
     fun env ->
      let rec loop result = function
        | [] -> result
        | a :: rest ->
            (match result with
	       BoolV false -> loop (a env) rest
	     | e ->  e)
      in loop (BoolV false) args
 
  | LambdaExp (ids, (defs, exp)) ->
     let b = analyzeExp exp
     and dd = List.map (fun (id, ex) -> id, analyzeExp ex) defs in
     fun env ->
      ProcV (ids, dd, b, env)

  | ApplyExp (exp, args) ->
     let proc = analyzeExp exp
     and a = List.map analyzeExp args in
     (fun env ->
       let aa = List.map (fun proc -> proc env) a in 
        eval_apply (proc env) aa)
(*
  | LetExp (binds, (defs, exp)) ->
      let a = extendlet env binds in
      let b = extendletrec a defs in
      evalExp b exp
 *)
  | NamedLetExp (id, binds, body) ->
      let (ids, args) = List.split binds
      and pid = analyzeExp (VarExp id) in
      let fn = LambdaExp (ids, body) in
      let ddd = [id, analyzeExp fn]
      and arr = List.map analyzeExp args in
      fun env ->
        let a = extendletrec env ddd in
	eval_apply (pid a) (List.map (fun proc -> proc env) arr)

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
	FUN (cond, ret, alt) ->
	let pcond = analyzeExp cond
	and pret = analyzeExp ret
	and palt = analyzeExp alt in
	fun env -> (match pcond env with
		      BoolV false -> palt env
		    | e -> eval_apply (pret env) [e])
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

  | BeginExp explist ->
     let a = List.map analyzeExp explist in
     fun env ->
      let result = ref UnitV in
      List.iter (fun x -> result := x env) a;
      !result
 

and eval_apply proc args =
  (match proc with
     ProcV (ids, defs, exp, en) -> (* procには定義リストもある  *)
     let newenv = extend en ids args in 
     let newnewenv = extendletrec newenv defs in
     exp newnewenv
   | PrimV closure ->
      closure args
   | _  -> failwith "not proc"
  )

and extendletrec env binds : env =
  let rec ext = function
      [] -> env
    | (id, _) :: rest -> (id, ref UnboundV) :: ext rest in
  let newenv = ext binds in
  let rec loop e = function
      [] -> ()
    | (_, exp) :: rest  ->
	let (_, v) :: r = e in
        v := exp newenv; loop r rest
  in
  loop newenv binds;
  newenv







let minus (ls :valtype list) =
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

let nullp = function
    EmptyListV -> true
  | _ ->  false

(*
let display a =
    print_string (printval a); flush stdout; UnitV
 *)
let newline = function
    []  -> print_newline (); UnitV
  | _ -> failwith "Arity mismatch: newline"



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
  ("<", let rec apply = function
    | [IntV i; IntV j] -> i < j 
    | _ -> failwith "Arity mismatch: ="
	in fun ls -> BoolV (apply ls));
  (">", let rec apply = function
    | [IntV i; IntV j] -> i > j 
    | _ -> failwith "Arity mismatch: ="
	in fun ls -> BoolV (apply ls));
  ("cons", function
     | [a;b] -> cons a b
     | _ -> failwith "Arity mismatch: cons");
  ("car", function
     | [x] -> car x
     | _ -> failwith "Arity mismatch: car");
  ("cdr", function
     | [x] -> cdr x
     | _ -> failwith "Arity mismatch: cdr");
  ("null?", function
       [x] -> BoolV (nullp x)
     | _ -> failwith "Arity mismatch: null?");
  ("list", 
   let rec makelist = function
       [] -> EmptyListV
     | a :: rest -> cons a (makelist rest) in
   fun l -> makelist l);
  ("caar", function
     | [x] -> car (car x)
     | _ -> failwith "Arity mismatch: caar");
  ("map", function
       [proc; l] ->
       let rec map = function
	 | EmptyListV -> EmptyListV
	 | PairV(x, rest) ->
            PairV (ref (eval_apply proc [!x]), ref (map !rest))
	 | _ -> failwith "not pair: map"
       in map l

     | _ -> failwith "Arity mismatch: map");
(*
  ("write", function 
       [a] -> display a
     | _ -> failwith "Arity mismatch: write");
  ("display", function 
       [a] -> display a
     | _ -> failwith "Arity mismatch: display");
 *)
  ("newline", newline);

	    ]


let parse s =
  parseExp (Sparser.sexpdata Lexer.main (Lexing.from_string s))


let analyze s = 
  analyzeExp (parse s)

let evallex sexps =
  let (defs, exp) = Parser.parseBody sexps in
  let dd = List.map (fun (id, ex) -> (id, analyzeExp ex)) defs in
  let envv = extendletrec primis dd in
    (analyzeExp exp) envv;;

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
    (evallex (lex_from name))
