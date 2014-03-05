(* oscheme parser.ml *)

open Syntax


type id = string;;


type exp =
  | SelfEvalExp of sexp
  | VarExp of id
  | UnitExp
  | QuoteExp of sexp
  | IfExp of exp * exp * exp
  | AndExp of exp list
  | OrExp of  exp list
  | LambdaExp of id list * body
  | ApplyExp of exp * exp list
  | LetExp    of (id * exp) list * body
  | NamedLetExp of id * (id * exp) list * body
  | LetrecExp of (id * exp) list * body
(*  | CONDexp of (exp * exp list) list *)
  | CondClauseExp of condclause
  | SetExp of id * exp
  | BeginExp of exp list
and define = id * exp
and body = define list * exp
and condclause = 
   ARROW of exp * exp * exp
 | VAL of exp * exp





(* 再帰下降パーサ *)

exception ParseError of string


let rec to_list a = function
    | List x -> a :: x
    | Cons (x, y) -> a :: to_list x y
    | _ -> raise (ParseError "not propper list")

let rec to_list2 = function
    | List x ->  x
    | Cons (x, y) -> x :: to_list2 y
    | x -> [x]


let rec parseIdlist : sexp list -> id list = function
  | [] -> []
  | Id id :: rest -> id :: parseIdlist rest
  | _ -> raise (ParseError "parseIdlist")


let body_to_exp = function
    [x] -> x
  | x -> BeginExp x

let rec parseExp : sexp -> exp = function
  | Int _ | Bool _ | Char _ | String _ as a -> SelfEvalExp a
  | Id x  -> VarExp x
  | List x -> parseForm x
  | Cons _ -> raise (ParseError "dot pair") 
   (* sparserが できる限りListにしているのに依存 *)
(*
  | Cons (x, y) ->
       parseForm (to_list x y)
 *)

and parseForm : sexp list -> exp = function
  | Id "quote" :: rest ->
      (match rest with
        [a] -> QuoteExp a
      | _ -> raise (ParseError "quote"))
  | Id "if" :: rest ->
      (match rest with
        [pred; conseq; alt] ->
          IfExp (parseExp pred, parseExp conseq, parseExp alt)
       |[pred; conseq] ->
          IfExp (parseExp pred, parseExp conseq, UnitExp)
       | _ -> raise (ParseError "if") )
  | Id "and" :: rest ->
      let rec make = function
          [] -> SelfEvalExp (Bool true)
        | [x] -> parseExp x
        | x :: rest -> IfExp (parseExp x, make rest, SelfEvalExp (Bool false)) in
      make rest
  | Id "and_" :: rest -> AndExp (parseExplist rest)
  | Id "or" :: rest -> OrExp (parseExplist rest)
  | Id "lambda" :: rest ->
      (match rest with
        List idlist :: rest ->
          let ids = parseIdlist idlist and body = parseBody rest in
          LambdaExp (ids, body)
      | _ -> raise (ParseError "lambda"))
  | Id "cond" :: rest ->
      parseClauses' rest
  | Id "let_" :: rest ->
      (match rest with
      | Id var :: List binds :: body ->
          let a = parseBinding binds and b = parseBody body in
	  NamedLetExp (var, a, b)
      | List binds :: body ->
          let a = parseBinding binds and b = parseBody body in
          LetExp (a, b)
      | _ -> raise (ParseError "let"))
  | Id "let" :: rest ->
      (match rest with
      | Id var :: List binds :: body ->
          let (ids, args) = unzipBindings binds in
          let fn = LambdaExp (ids, parseBody body) in
	  let l = LetrecExp ([var, fn], ([], VarExp var)) in
          ApplyExp (l, args)
(*
      | Id var :: List binds :: body ->
          let (ids, args) = unzipBindings binds and b = parseBody body in
          let fn = LambdaExp (ids, b) in
          LetrecExp ([var, fn], ([], ApplyExp (VarExp var, args)))
*)
      | List binds :: body ->
          let (ids, args) = unzipBindings binds in
	  let fn = LambdaExp (ids, parseBody body) in
          ApplyExp (fn, args)
      | _ -> raise (ParseError "let"))
  | Id "letrec" :: rest ->
      (match rest with
        List binds :: b ->
          let a = parseBinding binds in
          LetrecExp (a, parseBody b)
      | _ -> raise (ParseError ""))
  | Id "set!" :: rest ->
      (match rest with 
        [Id id; ex] -> SetExp (id, parseExp ex)
      | _ -> raise (ParseError "set!"))
  | Id "begin" :: rest ->
      BeginExp (parseExplist rest)
  | Id "delay" :: rest ->
      (match rest with 
        [ex] -> LambdaExp ([], ([],parseExp ex))
      | _ -> raise (ParseError "force"))
  | op :: rest ->
       let opp = parseExp op and args = parseExplist rest in
       ApplyExp (opp, args)
  | [] -> raise (ParseError "empty form")

and parseExplist (l : sexp list) =
  List.map parseExp l

and parseBinding (l: sexp list) : (id * exp) list =
  List.map (function
	       | List [Id id; ex] -> (id, parseExp ex)
	       | _ -> raise (ParseError "bindings")) l

and unzipBindings : sexp list -> (id list * exp list) = function
    [] -> [],[]
  | List [Id id; ex] :: rest -> 
      let (ids, x) = unzipBindings rest in
      (id :: ids), (parseExp ex :: x)
  | _ ->raise (ParseError "bindings")

and parseClauses' = function
    [] -> UnitExp  (* else節なし  *)
  | List e :: rest ->
      (match e, rest with
        Id "else" :: body,  [] -> body_to_exp (List.map parseExp body)
      | Id "else" :: _,     _  -> raise (ParseError "else clause is not last")
      | [cond; Id "=>"; fn],_  ->
	  CondClauseExp (ARROW (parseExp cond, parseExp fn, parseClauses' rest))
      | cond :: [],         _  ->
	  CondClauseExp (VAL (parseExp cond, parseClauses' rest))
      | cond :: body, _  ->
          let v = body_to_exp (List.map parseExp body) in
	  IfExp (parseExp cond, v, parseClauses' rest))
  | _ -> raise (ParseError "cond clause")


and parseClauses = function
    [] -> [] (* else節なし  *)
  | List e :: rest ->
      (match e, rest with
        Id "else" :: body,  [] -> (SelfEvalExp (Bool true), List.map parseExp body) :: []
      | Id "else" :: _,     _  -> raise (ParseError "else clause is not last")
      | [cond; Id "=>"; fn],_  ->
          let condexp = parseExp cond in
          let app = ApplyExp (parseExp fn, [condexp]) in
          let xy = (condexp, [app]) in
          xy :: parseClauses rest
      | cond :: body,       _  -> 
	  let xy = (parseExp cond, List.map parseExp body) in
	  xy :: parseClauses rest
      | [],                 _  -> failwith "not concidered")
  | _ -> raise (ParseError "cond clause")

(*
and clausestoif clauses = 
  List.fold_right (fun (e, body) y -> IfExp (e, body_to_exp body, y))
                  (parseClauses clauses)
                  (UnitExp)
 *)

(* 定義は[本体]の先頭で有効 *)
and parseDef : sexp list -> define = function
  | List (Id id :: rest) :: l -> 
      let ids = parseIdlist rest and body = parseBody l in
      id, LambdaExp (ids, body)
  | Cons (Id id, rest) :: l ->
     raise (ParseError "(define dot list ")
(*
     let a = to_list2 rest in
      let ids = parseIdlist rest and body = parseBody l in
      id, LambdaExp (ids, body)
 *)
  | [Id id; ex] -> 
      (* let c = parseExp ex in *)
      id, parseExp ex
  | _ -> raise (ParseError " not (define ")

and parseBody : sexp list -> body = function
  | [] -> raise (ParseError " parse body ")
  | List (Id "define" :: x) :: rest ->
      let def = parseDef x and (defs, e) = parseBody rest in
      (def :: defs), e
  | [exp] -> [], parseExp exp
  | exl -> let a = List.map parseExp exl in
	   [], BeginExp a
  (* | l :: rest -> let (c) = parseExp l in ([], c)  *);;



(* for mininterpreter *)

type func = id * id list * exp
type var = id * int

type prog = func list * var list * exp

type def = Func of func | Var of var

let parseFunc : sexp list -> def  = function
  | [List (Id id :: rest); exp] -> 
      Func (id, parseIdlist rest, parseExp exp)
  | [Id id; Int n] -> Var (id, n)
  | _ -> raise (ParseError " not (define ")

let rec parseProg : sexp list -> prog = function
    [] -> raise (ParseError " parseProg ")
  | List (Id "define" :: x) :: rest ->
       let def = parseFunc x and (fl, vl, exp) = parseProg rest in
       (match def with
         Func f -> (f :: fl, vl, exp)
       | Var v -> (fl, v :: vl, exp))
  | ex :: _ -> let c = parseExp ex in ([], [], c);;

