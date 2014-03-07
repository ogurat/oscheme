


open Valtype
open Scheme


type 'a v = V of 'a valtype | Ex of string

(*
let a = (SymbolV "a") and b = (SymbolV "b")
let pairab =  PairV ( a,  b)
 *)
let proc =  ProcV (["x";"y"], Fixed, [], (fun _ -> UnitV), [])
let procnn =  ProcV (["nn"], Fixed, [], (fun _ -> UnitV), [])

let bcase = ("../scm/b.scm", [


("((lambda (x) (* x x)) 4)", Ex "16");

("(varf 'a 'b 'c 'd 'e)", Ex "'(c d e)" );
("(varf2 1 2 3 4)",  V (IntV 10));
("(varf3 'a 'd 'g)", Ex "'((a d g) a d g)");

("c", Ex "'((b (c . d)) (b (c d)) (b (c d)) (b (c d)) (b (c d)) (b c d) (b c d) (b c d) (b c d) (b c d) (b c d) (b c d) (b c d) (b c . d) (b c . d))");

("(dotp 'a 'b 'c)", Ex "'(c c c a a (a . b) (a . b) (a . b))");
("(dotp #f 'a 'b)", Ex "'(#f #f #f a a (#f . a) (#f . a) (#f . a))");

("((aaa 2 3))", V (IntV 5)) ;
("((aaa 3 2))", V (IntV 6)) ;
("(a1 5)", Ex "'((#f #f #f #t) (120 120 120))");
("(a1 4)", Ex "'((#t #t #t #f) (24 24 24))");
("(a2 4 5)", Ex "'(4 9 13 22 29 4 20 80 400)");

("(testand)", Ex "'((1 2 #t #f) (1 1 5 7 #t))");


("(cons 'a 'b)", Ex "'(a . b)");

("(xtest 3 8)", Ex "'(24 9 64)");
("(condtest 1 'b)", Ex "'(first  (edf ghi) 2 (b 2) (y z) xyz)");
("(condtest 2 'c)", Ex "'(second  (edf ghi) else2 else3 (y z) xyz)");

])

let lettestcase = ("../scm/lettest.scm", [


("(fibs)",  Ex "'((3 5 2584 4181 6765) (3 5 2584 4181 6765) (3 5 2584 4181 6765) (3 5 2584 4181 6765) (16 25 324 361 400))");

("(ff 10 5)", Ex "'(65 50 750 9)");
("(mapsquare '(3 4 5))", Ex "'(9 16 25)");

	       ])

let bodycase = ("../scm/body.scm", [

"(a1 5 4)", V (IntV 20) ;
"(a3 5 4)", V (IntV 9) ;
"(a3 4 5)", V (SymbolV "q") ;

"(a4 1 2)", V (SymbolV "c") ;
"(a4 2 1)", V (SymbolV "y") ;
"(a4 2 2)", V (SymbolV "h") ;

"(fib 6)",  V (IntV 8) ;
"(a7 3 4)", V (IntV 12) ;
"(a7 4 3)", V (IntV 7) ;

	       ])

let sicp = ("../scm/sicp4.scm", [
  "(replanalyze)", Ex "'()"
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


let withfile proc name =
  let f = open_in name in
  try
    let a = proc f in
    close_in f; a
  with Failure msg -> close_in f; raise (Failure msg)


let sexps_from =
  withfile (fun f -> Sparser.toplevel Lexer.main (Lexing.from_channel f))


(*
let interpret name =
   Scheme.printval (evalall (sexps_from name))
 *)


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




let  atest (file, cases) =
  let primis = ge Analyze.eval_apply
  and (defs, _) = Parser.parseDefs (sexps_from file) in
  let dd = List.map (fun (id, ex) -> (id, Analyze.analyzeExp ex)) defs in
  let env = Analyze.extendletrec primis dd in
 List.map (exec Analyze.analyzeExp env) cases


let  etest (file, cases) =
  let primis = ge Eval.eval_apply
  and (defs, _) = Parser.parseDefs (sexps_from file) in
  let env = Eval.extendletrec primis defs in
 List.map (exec (fun exp en-> Eval.evalExp en exp) env) cases

let show_exp (file, _) =
  let (defs, _) = Parser.parseDefs (sexps_from file) in
  defs

let def_exp name =
  List.assoc name
