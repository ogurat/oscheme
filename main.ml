

open Scheme


let sexps_from name =
  let f = open_in name in
  try
    let s = Sparser.toplevel Lexer.main (Lexing.from_channel f) in
    close_in f; s
  with Failure msg -> close_in f; raise (Failure msg)




let eeval (defs, exp) =
  let primis = ge Eval.eval_apply in
  let env = Eval.extendletrec primis defs in
    Eval.evalExp env exp

let aeval (defs, exp) =
  let primis = ge Analyze.eval_apply
  and dd = List.map (fun (id, ex) -> (id, Analyze.analyzeExp ex)) defs in
  let env = Analyze.extendletrec primis dd in
    Analyze.analyzeExp exp env

(*
let einterpret name =
     let v = Parser.parseBody (sexps_from name) in
     eeval v
 *)

let _ =

  let fn = ref [] and ev = ref false in
  let spec = "-eval", Arg.Set ev, "eval evaluator" in
  Arg.parse [spec] (fun s -> fn := s :: !fn) "";
   if List.length !fn > 0 then
     let v = Parser.parseBody (sexps_from (List.hd !fn)) in
     let a =
       if !ev then
         printval (eeval v)
       else
	 printval (aeval v) in
     print_endline a
   else ()


(*
ocaml -rectypes sparser.cmo parser.cmo lexer.cmo valtype.cmo eval.cmo analyze.cmo scheme.cmo main.cmo
*)
