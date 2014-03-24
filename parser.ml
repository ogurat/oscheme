(* oscheme parser.ml *)

open Syntax


type id = string;;

type varid =
   Fixed
 | Vararg of id


type exp =
  | SelfEvalExp of sexp
  | VarExp of id
  | UnitExp
  | QuoteExp of sexp
  | QuasiQuoteExp of qqexp
  | IfExp of exp * exp * exp
  | AndExp of exp list
  | OrExp of  exp list
  | LambdaExp of id list * varid * exp
  | ApplyExp of exp * exp list
  | LetrecExp of (id * exp) list * exp
  | DoExp of (id list * exp list * exp list) * exp * exp * exp list
  | CondClauseExp of condclause
  | SetExp of id * exp
  | SeqExp of exp * exp
and define = id * exp
(* and body = define list * exp *)
and condclause = 
   ARROW of exp * exp * exp
 | VAL of exp * exp
and qqexp =
    S of sexp
  | Unquote of exp
  | UnquoteSplice of exp
(*
  | L of qqexp list
 *)
  | P of qqexp * qqexp
  | Empty

type body = define list * exp


let rec to_string : (sexp list -> string) = function 
    [] -> ""
  | [x] -> to_string2 x
  | x :: rest -> to_string2 x ^ " " ^ to_string rest
and to_string2 = function
  | Int x  -> string_of_int x
  | Id x  -> x
  | List x  -> "(" ^ to_string x ^ ")"
  | Cons (x, y) ->
     (match y with 
       Cons _ ->  "(" ^ to_string2 x ^ " " ^ to_string2 y ^ ")"
     | _ -> "(" ^ to_string2 x ^ " . " ^ to_string2 y ^ ")"
     )
  | _ -> ""

  

(* 再帰下降パーサ *)

exception ParseError of string (* todo: s expressionを入れる errorをvariant化する *)


let rec to_list a = function
    | List x -> a :: x
    | Cons (x, y) -> a :: to_list x y
    | _ -> raise (ParseError "not propper list")

let rec to_list2 = function
    | List x ->  x
    | Cons (x, y) -> x :: to_list2 y
    | x -> [x]



let rec parseIdCons = function
(*    | Nil -> ([], Fixed) *)
  | Cons (Id id, y) ->
     let (a, b) = parseIdCons y in (id :: a, b)
  | Id id -> [], Vararg id
  | _ -> raise (ParseError "parseIdCons")

let rec parseIdlist : sexp list -> id list = function
  | [] -> []
  | Id id :: rest -> id :: parseIdlist rest
  | _ -> raise (ParseError "parseIdlist")

let rec body_to_exp = function
    [] -> raise (ParseError "empty body")
  | [x] -> x
  | x :: rest -> SeqExp (x, body_to_exp rest)

let rec splitBindings = function
    [] -> [],[]
  | List [id; ex] :: rest -> 
      let (ids, x) = splitBindings rest in
      (id :: ids), (ex :: x)
  | _ ->raise (ParseError "bindings")

let rec unzipBindings parse : sexp list -> (id list * exp list) = function
    [] -> [],[]
  | List [Id id; ex] :: rest -> 
      let (ids, x) = unzipBindings parse rest in
      (id :: ids), (parse ex :: x)
  | _ ->raise (ParseError "bindings")


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
      | _ -> raise (ParseError "quote")
      )
  | Id "quasiquote" :: rest ->
      (match rest with
        [a] ->
          (match parse_qq 0 a with
             UnquoteSplice _ -> raise (ParseError ("qq splice: " ^ to_string rest))
           | x -> QuasiQuoteExp (x)
          )
      | qq -> raise (ParseError ("quasiquote: " ^ to_string qq))
      )
  | Id "unquote" :: x | Id "unquote-splicing" :: x ->
           raise (ParseError ("unquote not in qq: " ^ to_string x))
  | Id "if" :: rest ->
     (match rest with
       pred ::conseq :: alt ->
         let altexp = (match alt with
           [x] -> parseExp x
         | [] -> UnitExp
         | _ -> raise (ParseError "if")
         ) in 
         IfExp (parseExp pred, parseExp conseq, altexp)
     | _ -> raise (ParseError "if")
     )
  | Id "and" :: rest ->
      let rec make = function
          [] -> SelfEvalExp (Bool true)
        | [x] -> parseExp x
        | x :: rest ->
           IfExp (parseExp x, make rest, SelfEvalExp (Bool false)) in
      make rest
  | Id "and_" :: rest -> (* derived s expression  *)
     (match rest with
	[] -> SelfEvalExp (Bool true)
        | [x] -> parseExp x
        | x :: rest ->
           (* (if x (and rest) #f) *)
           let a = List ([Id "if"; x; List (Id "and" :: rest); Bool false]) in
           parseExp a
     )
  | Id "and__" :: rest -> AndExp (parseExplist rest)
  | Id "or" :: rest -> OrExp (parseExplist rest)
  | Id "lambda" :: rest ->
      (match rest with
        List idlist :: rest ->
          let ids = parseIdlist idlist in
          LambdaExp (ids, Fixed, parseBodyLetrec rest)
      | Id id :: rest ->
          LambdaExp ([], Vararg id, parseBodyLetrec rest)
      | Cons _ as x :: rest ->
          let (ids, varid) = parseIdCons x in
          LambdaExp (ids, varid, parseBodyLetrec rest)
      | _ -> raise (ParseError "lambda")
      )
  | Id "cond" :: rest ->
      parseClauses' rest
  | Id "let"   :: rest -> parseLet rest
(*
  | Id "let_"  :: rest -> parseLet_ rest
  | Id "let__" :: rest -> parseLet__ rest
 *)
  | Id "letrec" :: rest ->
      (match rest with
        List binds :: b ->
          let a = parseBinding binds in
          LetrecExp (a, parseBodyLetrec b)
      | _ -> raise (ParseError "letrec: empty")
      )
  | Id "set!" :: rest ->
      (match rest with 
        [Id id; ex] -> SetExp (id, parseExp ex)
      | _ -> raise (ParseError "set!")
      )
  | Id "begin" :: rest ->
      body_to_exp (parseExplist rest)
  | Id "do" :: rest -> parseDo_ rest
  | Id "delay" :: rest ->
      (match rest with 
        [ex] ->  LambdaExp ([], Fixed, parseExp ex)
      | _ -> raise (ParseError "delay")
      )
  | Id "delay_" :: rest -> (* derived s expression  *)
      (match rest with 
        [ex] ->
          (* (lambda () ex) *)
	  let x = List [Id "lambda"; List []; ex] in parseExp x
      | _ -> raise (ParseError "delay")
      )
  | op :: rest ->
      let opp = parseExp op and args = parseExplist rest in
      ApplyExp (opp, args)
  | [] -> raise (ParseError "empty form")


and parseLet = function
  | Id var :: List binds :: body ->
     let (ids, args) = unzipBindings parseExp binds in
     let fn = LambdaExp (ids, Fixed, parseBodyLetrec body) in
     ApplyExp (LetrecExp ([var, fn], VarExp var), args)
(*
  | Id var :: List binds :: body ->
     let (ids, args) = unzipBindings parseExp binds in
     let fn = LambdaExp' (ids, Fixed, parseBodyLetrec body) in
     LetrecExp ([var, fn], ([], ApplyExp (VarExp var, args)))
 *)
  | List binds :: body ->
     let (ids, args) = unzipBindings parseExp binds in
     let fn = LambdaExp (ids, Fixed, parseBodyLetrec body) in
     ApplyExp (fn, args)
  | _ -> raise (ParseError "let")

(* derived s expression  *)
and parseLet_ = function
  | Id var :: List binds :: body ->
     let (ids, args) = splitBindings binds in
     (* ((letrec
           ((var (lambda ids body)))
           var) args) *)
     let x = List (List [Id "letrec";
                         List [List [Id var; List (Id "lambda" :: List ids :: body)]];
                         Id var] :: args) in
     parseExp x
  | List binds :: body ->
     let (ids, args) = splitBindings binds in
     (* ((lambda ids body) args) *)
     let x = List (List (Id "lambda" :: List ids :: body) :: args) in
     parseExp x
  | _ -> raise (ParseError "let")


and parseDo = function (* todo:外側の変数loopを隠してしまう *)
  | List specs :: test_exps :: commands -> (* testが成立すると, expsを評価してdoを抜ける *)
     let (vars, inits, steps) = splitSpecs specs
     and test, exps = (match test_exps with 
		| List (test :: exps) -> (parseExp test, parseExplist exps)
		| _ -> raise (ParseError "do: test spec"))
     and cmds = parseExplist commands in
(* (letrec
     ((loop (lambda (vars) (if test (begin exps) (begin commands (loop steps))))))
     (loop inits)
  *)
     let apploop = ApplyExp (VarExp "loop", steps) in
     let loopbody = IfExp (test, body_to_exp exps, body_to_exp (cmds @ [apploop])) in
     let loop = LambdaExp (vars, Fixed, loopbody) in
     LetrecExp (["loop", loop], ApplyExp (VarExp "loop", inits))
 
  | _ -> raise (ParseError "do ")

and parseDo_ = function
  | List specs :: test_exps :: commands -> (* testが成立すると, expsを評価してdoを抜ける *)
     let specs = splitSpecs specs
     and test, exps = (match test_exps with 
		| List (test :: exps) -> (parseExp test, parseExplist exps)
		| _ -> raise (ParseError "do: test spec"))
     and cmds = parseExplist commands
      in
(* (letrec
     ((loop (lambda (vars) (if test (begin exps) (begin commands (loop steps))))))
     (loop inits)
  *)
     DoExp (specs, test, body_to_exp exps, cmds)
 
  | _ -> raise (ParseError "do ")

and splitSpecs  : sexp list ->  (id list * exp list * exp list)  = function
    [] -> [],[],[]
  | List [Id var; init; step] :: rest -> 
      let (vars, inits, steps) = splitSpecs rest in
      (var :: vars), (parseExp init :: inits), (parseExp step :: steps)
  | List [Id var; init] :: rest -> 
      let (vars, inits, steps) = splitSpecs rest in
      (var :: vars), (parseExp init :: inits), (VarExp var :: steps)
  | _ ->raise (ParseError "do specs")

and parseExplist (l : sexp list) =
  List.map parseExp l

and parseBinding (l: sexp list) : (id * exp) list =
  List.map (function
             | List [Id id; ex] -> (id, parseExp ex)
             | _ -> raise (ParseError "bindings")) l



and parseClauses' = function
    [] -> UnitExp  (* else節なし  *)
  | List e :: rest ->
      (match e, rest with
        Id "else" :: body,  [] ->
          body_to_exp (List.map parseExp body)
      | Id "else" :: _,     _  ->
          raise (ParseError "else clause is not last")
      | [cond; Id "=>"; fn],_  ->
          CondClauseExp (ARROW (parseExp cond, parseExp fn, parseClauses' rest))
      | cond :: [],         _  ->
          CondClauseExp (VAL (parseExp cond, parseClauses' rest))
      | cond :: body, _  ->
          let v = body_to_exp (List.map parseExp body) in
          IfExp (parseExp cond, v, parseClauses' rest)
      | [], _ -> raise (ParseError "cond clause")
      )
  | _ -> raise (ParseError "cond clause")
(*
and parseQQ level : sexp -> qqexp = function
  | Syntax.List x ->
     (match x with
        [Syntax.Id "quasiquote"; a] ->
           L (List.map (parseQQ (level + 1)) x)
      | [Syntax.Id "unquote"; a] ->
         if level = 0 then
           Unquote(parseExp a)
         else
           L (List.map (parseQQ (level - 1)) x)
      | [Syntax.Id "unquote-splicing"; a] ->
         if level = 0 then
           UnquoteSplice(parseExp a)
         else
           L (List.map (parseQQ (level - 1)) x)
      | _ -> L (List.map (parseQQ level) x)
     )
  | x -> S x
 *)
and parse_qq level : sexp -> qqexp = function
  | Syntax.List x ->
     let rec loop level = function
         [] -> Empty
(*
      | Syntax.Id "quasiquote":: _ ->
         raise (ParseError "quasiquote format")
 *)
      | [Syntax.Id "unquote" as u; a] ->
         if level = 0 then
           Unquote(parseExp a)
         else
           P (parse_qq (level - 1) u, P (parse_qq (level - 1) a, Empty))
      | Syntax.Id "unquote" :: _ ->
         raise (ParseError "unquote format")
      | Syntax.Id "unquote-splicing" :: _ ->
         raise (ParseError "unquote-splicing format")

      | a :: rest -> P (parse_qq level a, loop level rest) in
     (match x with
      | [Syntax.Id "quasiquote" as u; a] ->
         P (parse_qq (level + 1) u, P (parse_qq (level + 1) a, Empty))
      | [Syntax.Id "unquote-splicing" as u; a] ->
         if level = 0 then
           UnquoteSplice(parseExp a)
         else
           P (parse_qq (level - 1) u, P (parse_qq (level - 1) a, Empty))
      | _ ->
         loop level x
     )
  | x -> S x

(*
and parseQuasi depth : sexp -> sexp = function
  | Syntax.List [Syntax.Id "unquote"; form] -> form
  | Syntax.List [Syntax.List (Syntax.Id "unquote-splicing":: form :: rest)] ->
     append form (Syntax.List (Syntax.Id "quasiquote" :: rest))
 *)
and parseClauses = function
    [] -> [] (* else節なし  *)
  | List e :: rest ->
      (match e, rest with
        Id "else" :: body,  [] ->
          (SelfEvalExp (Bool true), List.map parseExp body) :: []
      | Id "else" :: _,     _  ->
          raise (ParseError "else clause is not last")
      | [cond; Id "=>"; fn],_  ->
          let condexp = parseExp cond in
          let app = ApplyExp (parseExp fn, [condexp]) in
          let xy = (condexp, [app]) in
          xy :: parseClauses rest
      | cond :: body,       _  -> 
          let xy = (parseExp cond, List.map parseExp body) in
          xy :: parseClauses rest
      | [],                 _  -> failwith "not considered"
      )
  | _ -> raise (ParseError "cond clause")

(*
and clausestoif clauses = 
  List.fold_right (fun (e, body) y -> IfExp (e, body_to_exp body, y))
                  (parseClauses clauses)
                  (UnitExp)
 *)

(* 定義は[本体]の先頭で有効 *)
and parseDef : sexp list -> define = function
  (* (define (var formals) exps *)
  | List (Id var :: formals) :: exps ->
      let ids = parseIdlist formals and body = parseBodyLetrec exps in
      var, LambdaExp (ids, Fixed, body)
  | Cons (Id var, x) :: exps ->
      let (ids, varid) = parseIdCons x and body = parseBodyLetrec exps in
      var, LambdaExp (ids, varid, body)
  (* (define id ex *)
  | [Id var; ex] ->
      var, parseExp ex
  | _ -> raise (ParseError "define: invalid form")

and parseBody : sexp list -> body = function
  | [] -> raise (ParseError "parse body: empty body")
  | List (Id "define" :: x) :: rest ->
      let def = parseDef x and (defs, e) = parseBody rest in
      def :: defs, e
  | exl -> let a = List.map parseExp exl in
           [], body_to_exp a

and parseBodyLetrec sexps : exp =
  let (defs, exp) = parseBody sexps in
  match defs with
    [] -> exp
  | x -> LetrecExp (defs, exp)


let rec parseDefs = function
  | List (Id "define" :: x) :: rest ->
      let (id, exp) = parseDef x and (defs,l) = parseDefs rest in
      (id, exp) :: defs, l
  | exl -> [], List.map parseExp exl



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

