
open Parser
open Scheme


let withfile proc name =
  let f = open_in name in
  try
    let a = proc f in
    close_in f; a
  with Failure msg -> close_in f; raise (Failure msg)


let sexps_from =
  withfile (fun f -> Sparser.toplevel Lexer.main (Lexing.from_channel f))



let eeval sexps =
  let (defs, exp) = parseBody sexps
  and primis = ge Eval.eval_apply in
  let env = Eval.extendletrec primis defs in
  Eval.evalExp env exp

let aeval sexps =
  let (defs, exp) = parseBody sexps
  and primis = ge Analyze.eval_apply in
  let dd = List.map (fun (id, ex) -> (id, Analyze.analyzeExp ex)) defs in
  let env = Analyze.extendletrec primis dd in
  Analyze.analyzeExp exp env


let aleval sexps =
  let primis = ge Eval.eval_apply
  and defs = parseDefs (sexps_from "lib/alexpander.scm") in
  let env = Eval.extendletrec primis defs in
  let expandproc = List.assoc "expand-program" defs in
  let expand = ApplyExp (expandproc, [QuoteExp (Syntax.List sexps)]) in
  let x = Eval.evalExp env expand in

  match Valtype.val_to_sexp x with 
    Syntax.List ss ->
      let (defs, exp) = parseBody ss in
      let env = Eval.extendletrec primis defs in
      Eval.evalExp env exp


(*
let einterpret name =
     let v = Parser.parseBody (sexps_from name) in
     eeval v
 *)

let _ =

  let fn = ref [] and ev = ref false and syntax = ref false in
  let specs = [ "-eval", Arg.Set ev, "eval evaluator";
"-syntax", Arg.Set syntax, "syntax-rules" ] in
  Arg.parse specs (fun s -> fn := s :: !fn) "";
   if List.length !fn > 0 then
     let v = (sexps_from (List.hd !fn)) in
     let a =
       if !ev then
         if !syntax then
           printval (aleval v)
         else
           printval (eeval v)
       else
	 printval (aeval v) in
     print_endline a
   else ()


(*
ocaml -I _build -rectypes sparser.cmo parser.cmo lexer.cmo valtype.cmo eval.cmo analyze.cmo scheme.cmo main.cmo
*)
