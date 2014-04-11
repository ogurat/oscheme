


open Valtype
open Scheme

open Parser
open Syntax


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

("(a7 '(3 -2 1 6 -5))", Ex "'((6 1 3) (-5 -2))" ) ;

("(testand)", Ex "'((1 2 #t #f) (1 1 5 7 #t))");


("(cons 'a 'b)", Ex "'(a . b)");

("(xtest 3 8)", Ex "'(24 9 64)");
("(cond1 1 'b)", Ex "'(first  (edf ghi) 2 (b 2) (y z) xyz)");
("(cond1 2 'c)", Ex "'(second  (edf ghi) else else (y z) xyz)");
("(cond2 1 'b)", Ex "'(first #t 2 (b 2))") ;
(*
let te =  V (PairV ( ref (BoolV true), ref (PairV (  ref UnitV, ref (PairV ( ref UnitV, ref EmptyListV  ))))))
 *)

("(case1 6)", Ex "'(composit composit)" ) ;
("(case1 10)", Ex "'(else composit)" ) ;
("(case2)", Ex "'(first second else)" ) ;

("a10",   V (StringV "\basd\007\r\n\t\"\\asd")) ;

])



let lettestcase = ("scm/lettest.scm", [


("(fibs)",  Ex "'((3 5 2584 4181 6765) (3 5 2584 4181 6765) (3 5 2584 4181 6765) (3 5 2584 4181 6765) (16 25 324 361 400))");

("(mapsquare '(3 4 5))", Ex "'(9 16 25)") ;
("(letlist 3 4)", Ex "'(9 8 5)") ;
("(letlist2 3 4)", Ex "'(9 8 17)") ;
("(letstar)", Ex "70") ;
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

("(qq2 'a)", Ex "'(list a 'a)") ;

("(qq4 '(a s d))", Ex "'(a (b (c (d (s d)))))" ) ;
("(qq4-2 '(a s d))", Ex "'(a b (c (d (s d))))" ) ;
("(qq4-3 '(a s d))", Ex "'(a quasiquote (b (c (d ,(s d)))))" ) ;
(* "(qq4-3 '(a s d))", Ex "'(a quasiquote (b (c (d (s d)))))" *)   

("(qq5)", Ex "'(a 3 4 5 6 b)") ;
("(qq5-1)", Ex "'(4 5 6)") ;
("(qq5-5)", Ex "'((foo 7) . cons)") ;
("(qq5-7)", Ex "'(foo 7 . cons)") ;
("(qq5-8)", Ex "'(foo 7 . cons)") ;

("(qq6)" , Ex "'(a `(b c ,(+ 1 2) ,(foo 4 d) e) f)") ;

("(qq7 'x 'y 'z)", Ex "'(a `(b c ,x ,'y d) z e)") ;
("(qq7-1 'x 'y)", Ex "'(a `(b c . ,x) y e)") ;
("(qq7-2 'x)", Ex "'(a `(b (c d . ,x)) e)") ;
("(qq7-3 'x)", Ex "'(a `(b (c d . ,(e . x))) f)") ;
("(qq7-5 'x)", Ex "'(a `(b `(c d . ,,x)) e)") ;
("(qq7-5-2 'x)", Ex "'(a `(b `(c d ,,x)) e)") ;
("(qq7-6 'x)",     Ex "'(a b quasiquote ,x)") ;
("(qq7-6 '(x y))", Ex "'(a b quasiquote ,(x y))") ;

("(qq7-8 'xy)", Ex "'(a `(b quasiquote (c unquote ,xy)) e)") ;
("(qq7-9)",  Ex "'(a `(b quasiquote (c . ,,x)) e)") ;
("(qq7-10)", Ex "'(1 ```,,@,3 4)") ;

("(qq8 5 6)", Exc (Failure "splice not list")) ;
("(qq10 1)", Ex "'(7 1)") ;

("(qq11 'x)", Ex "'((x b))") ;
("(qq12 '(d))", Ex "'(a b ((c d)))") ;
("(qq12_2)", Ex "'(a b ((c . 3)))") ;
("(quasiquote (a b ((c unquote x y))))", Exc (ParseError("unquote format"))) ; 

("(qq13 'a)",   Ex "'(foo . a)") ;
("(qq13_ 'a)",  Ex "'(foo . a)") ;
("(qq13__ 'a)", Ex "'(foo . a)") ;
("(qq13_ '(a b c))",  Ex "'(foo a b c)") ;
("(qq13__ '(a b c))", Ex "'(foo a b c)") ;
("(qq14 'a)", Ex "'a") ;
("(qq14 '(a b c))", Ex "'(a b c)") ;

("(quasiquote (foo (unquote-splicing x y)))", Exc (ParseError("unquote-splicing: invalid context"))) ; 

("`,@(list 1 2)", Exc (ParseError("qq splice: (unquote-splicing (list 1 2))")) ) ;

("`(,,(a b))", Exc (ParseError("unquote not in qq: (a b)"))  ) ;
("`(foo . ,@x)", Exc (ParseError("unquote-splicing: invalid context"))  );
("`(a . ,@'(a b))", Exc (ParseError "unquote-splicing: invalid context") ) ;
("`,(quasiquote 'x 'y)", Exc (ParseError("quasiquote: (quote x) (quote y)"))  ) ;
("(qq18 'x 'y)", Ex "'(x y)"  ) ;
("(qq19 'as 'd)", Ex "'(a quasiquote (,as ,d))"  ) ;
("(qq20)", Ex "'(`((unquote a b)))") ;
("(qq22 '(a s d))", Ex "'`(a b ,(a s d))") ;


           ])

let lib = ("scm/libtest.scm", [
"(a1 '(a b c d) 2)", Ex "'((c d) c)" ;
"(a2 '(a b c d) 'c)", Ex "'((c d) (c d) (c d))" ;
"(a2 '(1 2 3) 2)", Ex "'(#f (2 3) (2 3))" ;
"(a3 '((a 1) (b 2) (c 3)) 'c)", Ex "'((c 3) (c 3) (c 3))" ; 


"(maptest)", Ex "'((9 16 25) (5 10 16) (23 27 31) ((a x 1 asd) (s y 2 fgh) (d z 3 jkl)))" ;
"(a4 '(1 2 3 4))", Ex "'(10 10 (4 3 2 1))" ;

"(a5 \"abcdefg\" 4)", Ex "'(#t 7 #\\e)" ; 


            ] )

let macro = ("scm/mlib.scm", [

"(let1)", Ex "3" ;
"(let2)", Ex "'asd" ;
"(let3)", Ex "'asd" ;

"(aa 2 5)", Ex "'10" ;

"(cond0 3 'c)", Ex "'else" ;
"(dotest '(1 2 3 4) 'c)", Ex "'(10 c)" ;

"(cond1 1 'b)", Ex "'(first (b 2) else)" ;
"(cond1 2 'c)", Ex "'(second else else)" ;
"(cond1 3 'b)", Ex "'(else (b 2) else)" ;

"(case1 2)", Ex "'(prime composit first second else)" ;

	       ])

let al = ("scm/alexpander.scm", [

"(expand-program '((or a b c)))", Ex "'()"

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

let  expand s =
  let x = parse s 
    and primis = ge Eval.eval_apply in
  (* let envv = Analyze.extendletrec primis [] in *)
    Expand.expandExp primis x


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


let show_sexp (file, _) =
  let es = sexps_from file in
  List.map (function List [Id "define"; List (Id id :: formals); body] -> (id, body)) es

(*
let def_sexp id es =
  Parser.to_string2 (List.assoc id es)
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


let exec eval env (s, v) = (* v:期待結果 *)
  try 
    let vv = eval (parse s) env 
    and ab = (match v with
		Ex ex -> eval (parse ex) []
	      | V v -> v
             ) in
    (match vv with
       ProcV _ -> (s, printval vv, true) (* abがProcであるかcheckすべし *)
     | _ -> (s, printval vv, vv = ab)
    )
  with
     Match_failure _ as ex ->
       (s, Printexc.to_string ex, false)
   | ex ->
      (match v with
         Exc x -> (s, Printexc.to_string ex, ex = x)
       | _ -> (s, Printexc.to_string ex, false)
      )


let exec_expand eval env (var,s) =

    let o = (Expand.expandExp env s) in
    (var, o)
(*
  with
     Match_failure _ as ex ->
       Printexc.to_string ex
   | ex ->
        Printexc.to_string ex
 *)



let atest (file, cases) =
  let primis = ge Analyze.eval_apply
  and (defs, _) = Parser.parseDefs (sexps_from file) in
  let dd = List.map (fun (id, ex) -> (id, Analyze.analyzeExp ex)) defs in
  let env = Analyze.extendletrec primis dd in
 List.map (exec Analyze.analyzeExp env) cases


let etest (file, cases) =
  let primis = ge Eval.eval_apply
  and (defs, _) = Parser.parseDefs (sexps_from file) in
  let env = Eval.extendletrec primis defs in
 List.map (exec (fun exp en-> Eval.evalExp en exp) env) cases

let show_exp (file, _) =
  let (defs, _) = Parser.parseDefs (sexps_from file) in
  defs

let def_exp name =
  List.assoc name

let show_expand (file, _) =
  let primis = ge Eval.eval_apply
  and (defs, _) = Parser.parseDefs (sexps_from file) in
  let env = Eval.extendletrec primis defs in
 List.map (exec_expand (fun exp en-> Eval.evalExp en exp) env) defs




(*
ocaml -I _build -rectypes sparser.cmo parser.cmo lexer.cmo valtype.cmo eval.cmo analyze.cmo scheme.cmo expand.cmo test.cmo
*)
