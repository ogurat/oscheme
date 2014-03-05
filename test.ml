


open Valtype
open Scheme


type 'a v = V of 'a valtype | Ex of string

(*
let a = (SymbolV "a") and b = (SymbolV "b")
let pairab =  PairV ( a,  b)
 *)
let proc =  ProcV (["x";"y"], [], (fun _ -> UnitV), [])
let procnn =  ProcV (["nn"], [], (fun _ -> UnitV), [])

let cases = ("../scm/b.scm", [

(*
("fact", V procnn);
 *)


("((lambda (x) (* x x)) 4)", Ex "16");

("c", Ex "'((b (c . d)) (b (c d)) (b (c d)) (b (c d)) (b (c d)) (b c d) (b c d) (b c d) (b c d) (b c d) (b c d) (b c d) (b c d) (b c . d) (b c . d))");

("(dotp 'a 'b 'c)", Ex "'(c c c a a (a . b) (a . b) (a . b))");
("(dotp #f 'a 'b)", Ex "'(#f #f #f a a (#f . a) (#f . a) (#f . a))");

("(a1 5)", Ex "'((#f #f #f #t) (120 120 120))");
("(a1 4)", Ex "'((#t #t #t #f) (24 24 24))");
("(a2 4 5)", Ex "'(4 9 13 22 29 4 20 80 400)");

("(testand)", Ex "'((1 2 #t #f) (1 1 5 7 #t))");


("(cons 'a 'b)", Ex "'(a . b)");

("(xtest 3 8)", Ex "'(24 9 64)");
("(condtest 1 'b)", Ex "'(first  (edf ghi) 2 (b 2) (y z) xyz)");
("(condtest 2 'c)", Ex "'(second  (edf ghi) else2 else3 (y z) xyz)");

])

let cases2 = ("../scm/lettest.scm", [


("(fibs)",  Ex "'((3 5 2584 4181 6765) (3 5 2584 4181 6765) (3 5 2584 4181 6765) (3 5 2584 4181 6765) (16 25 324 361 400))");

("(ff 10 5)", Ex "'(65 50 750 9)");
("(mapsquare '(3 4 5))", Ex "'(9 16 25)");



	       ])


let sparse s = 
  Sparser.sexpdata Lexer.main (Lexing.from_string s)
let sp2 s =
  Sparser.toplevel Lexer.main (Lexing.from_string s)


let parse s =
  Parser.parseExp (Sparser.sexpdata Lexer.main (Lexing.from_string s))


let analyze s =
  Analyze.analyzeExp (parse s)

let  eval s =
  let x = parse s 
    and primis = ge Analyze.eval_apply in
  (* let envv = Analyze.extendletrec primis [] in *)
    Analyze.analyzeExp x primis


let sexps_from name =
  let f = open_in name in
  try
    let s = Sparser.toplevel Lexer.main (Lexing.from_channel f) in
    close_in f; s
  with Failure msg -> close_in f; raise (Failure msg)

let interpret name =
   Scheme.printval (evalall (sexps_from name))



let exec eval env (s, v) =
try 
  let vv = eval (parse s) env 
    and ab = (match v with
		Ex ex -> eval (parse ex) []
	      | V v -> v) in
  (try
       (s, printval vv,  vv = ab)
    with  Invalid_argument msg -> (s, printval vv, false))
with Failure msg -> (s, msg, false)

(*
let aexec env (s, v) =
try 
  let vv = Analyze.analyzeExp (parse s) env 
    and ab = (match v with
		Ex ex -> Analyze.analyzeExp (parse ex) []
	      | V v -> v) in
  (try
       (s, Scheme.printval vv,  vv = ab)
    with  Invalid_argument msg -> (s, Scheme.printval vv, false))

with Failure msg -> (s, msg, false)
 *)



let  atest (file, cases) =
  let primis = ge Analyze.eval_apply
  and  (defs, _) = Parser.parseBody (sexps_from file) in
  let dd = List.map (fun (id, ex) -> (id, Analyze.analyzeExp ex)) defs in
  let env = Analyze.extendletrec primis dd in
 List.map (exec Analyze.analyzeExp env) cases

(*
let eexec env (s, v) =
try 
  let vv = Eval.evalExp env (parse s) 
    and ab = (match v with
		Ex ex -> Eval.evalExp [] (parse ex)
	      | V v -> v) in
  (try
       (s, Scheme.printval vv,  vv = ab)
    with  Invalid_argument msg -> (s, Scheme.printval vv, false))

with Failure msg -> (s, msg, false)
 *)

let  etest (file, cases) =
  let primis = ge Eval.eval_apply
  and  (defs, _) = Parser.parseBody (sexps_from file) in
  let env = Eval.extendletrec primis defs in
 List.map (exec (fun exp en-> Eval.evalExp en exp) env) cases

let show_exp (file, _) =
  let (defs, _) = Parser.parseBody (sexps_from file) in
  defs

