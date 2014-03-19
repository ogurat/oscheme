


open Valtype
open Scheme

open Parser

type 'a v =
    V of 'a valtype
  | Ex of string
  | Exc of exn


let bcase = ("scm/b.scm", [



("(dotest '(1 2 3 4) 'c)", Ex "'(10 c)") ;

("(varf 'a 'b 'c 'd 'e)", Ex "'(c d e)" );
("(varf2 1 2 3 4)",  V (IntV 10));
("(varf3 'a 'd 'g)", Ex "'((a d g) a d g)");


("c", Ex "'((b (c . d)) (b (c d)) (b (c d)) (b (c d)) (b (c d)) (b c d) (b c d) (b c d) (b c d) (b c d) (b c d) (b c d) (b c d) (b c . d) (b c . d))");

("(dotp 'a 'b 'c)", Ex "'(c c c a a (a . b) (a . b) (a . b))");
("(dotp #f 'a 'b)", Ex "'(#f #f #f a a (#f . a) (#f . a) (#f . a))");


("(((closure 'a 'b) 'c 'd) 'e ' f)", Ex "'(a b c d e f)") ;
("(((closure2 'a 'b) 'c 'd) 'e ' f)", Ex "'(a b c d e f)") ;

("((aaa 2 3))", V (IntV 5)) ;
("((aaa 3 2))", V (IntV 6)) ;
("(a1 5)", Ex "'((#f #f #f #t) (120 120 120 120 120))");
("(a1 4)", Ex "'((#t #t #t #f) (24 24 24 24 24))");
("(a2 4 5)", Ex "'(4 9 13 22 29 4 20 80 400)");
("(a5 10 5)", Ex "'(65 750 750 750)");

("(testand)", Ex "'((1 2 #t #f) (1 1 5 7 #t))");


("(cons 'a 'b)", Ex "'(a . b)");

("(xtest 3 8)", Ex "'(24 9 64)");
("(condtest 1 'b)", Ex "'(first  (edf ghi) 2 (b 2) (y z) xyz)");
("(condtest 2 'c)", Ex "'(second  (edf ghi) else2 else3 (y z) xyz)");

])

let lettestcase = ("scm/lettest.scm", [


("(fibs)",  Ex "'((3 5 2584 4181 6765) (3 5 2584 4181 6765) (3 5 2584 4181 6765) (3 5 2584 4181 6765) (16 25 324 361 400))");

("(mapsquare '(3 4 5))", Ex "'(9 16 25)") ;
("(letlist 3 4)", Ex "'(9 8 5)") ;
("(mapf fourtimes)", Ex "'(16 20 72 76 80)") ;

	       ])

let bodycase = ("scm/body.scm", [

"(a1 5 4)", V (IntV 20) ;
"(a3 5 4)", V (IntV 9) ;
"(a3 4 5)", Ex "'q" ;

"(a4 1 2)", Ex "'c" ;
"(a4 2 1)", Ex "'y" ;
"(a4 0 2)", Ex "'g" ;
"(a4 2 2)", Ex "'h" ;

"(fib 6)",  V (IntV 8) ;
"(a7 3 4)", V (IntV 12) ;
"(a7 4 3)", V (IntV 7) ;

	       ])

let sicp = ("scm/sicp4.scm", [
  "(replanalyze)", Ex "'()"
	     ])

let qq = ("scm/quasiquote.scm", [

("(qq 5 6)", Ex "'((a b 11) (a b ((q 11))) 5 x)") ;

("(qq2 'a 'b)", Ex "'(list a 'a)") ;


("(qq5)", Ex "'(a 3 4 5 6 b)") ;
("(qq55)", Ex "'((foo 7) . cons)") ;
("(qq56 1)", Ex "'(foo . 1)") ;
("(qq6)" , Ex "'(a `(b ,(+ 1 2) ,(foo 4 d) e) f)") ;
("(qq7 'x 'y)", Ex "'(a `(b ,x ,'y d) e)") ;
("(qq71)", Ex "'(1 ```,,@,3 4)") ;

("(qq8 5 6)", Exc (Failure "splice not list")) ;
("(qq10 1)", Ex "'(7 1)") ;

("(qq11 'x)", Ex "'((x b))") ;
("(qq12 'd)", Ex "'(a b ((c . d)))") ; 
(*
("(qq13 'b)", Ex "'(a . b)") ; 
 *)
("`,@(list 1 2)", Exc (ParseError "qq splice")) ;

("`(,,(a b))", Exc (ParseError "unquote not in qq")) ;


           ])


let parse_err_case = [
  "(letrec)", (ParseError "letrec: empty")

]


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

let  eeval s =
  let x = parse s 
    and primis = ge Eval.eval_apply in
  (* let envv = Analyze.extendletrec primis [] in *)
    Eval.evalExp primis x



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




let pexec (s, v) =
  try 
    let vv = parse s in
    (s, "ok", true) 
  with 
    ex -> 
     (s, Printexc.to_string ex, ex = v)

let perr cases =
 List.map pexec cases

let perr () = perr parse_err_case


let exec eval env (s, v) =
  try 
    let vv = eval (parse s) env 
    and ab = (match v with
		Ex ex -> eval (parse ex) []
	      | V v -> v) in
    (match vv with
       ProcV _ -> (s, printval vv, true)
     | _ -> (s, printval vv, vv = ab)
    )
  with 
    ex -> 
     let (Exc x) = v in
     (s, Printexc.to_string ex, ex = x)



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


(*
ocaml -I _build -rectypes sparser.cmo parser.cmo lexer.cmo valtype.cmo eval.cmo analyze.cmo scheme.cmo test.cmo
*)
