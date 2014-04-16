(* oscheme parser.ml *)

open Syntax


type id = string;;

type varid =
   Fixed
 | Vararg of id


type exp =
  | SelfEvalExp of sexp
  | VarExp of id
  | UnspecifiedExp
  | QuoteExp of sexp
  | QuasiQuoteExp of qqexp
  | IfExp of exp * exp * exp
(*
  | AndExp of exp list
  | OrExp of  exp list
 *)
  | LambdaExp of id list * varid * exp
  | ApplyExp of exp * exp list
  | MacroAppExp of id * sexp list
  | LetrecExp of (id * exp) list * exp
  | DoExp of (id list * exp list * exp list) * exp * exp * exp list
  | CondArrow of exp * exp * exp
  | CondVal of exp * exp (*   *)
(*  | CondClauseExp of condclause *)
  | SetExp of id * exp
  | SeqExp of exp * exp
  | MacroExp of id list * varid * exp
and define = id * exp
(* and body = define list * exp *)
and qqexp =
    S of sexp (* s expression *)
  | Unquote of exp
  | UnquoteSplice of exp
(*
  | L of qqexp list
 *)
  | P of qqexp * qqexp (* cons pair *)
  | Nil

type body = define list * exp

type macro = id list * varid * exp (* macroのみの環境(namespace)を想定  *)

let rec string_of : (sexp list -> string) = function 
    [] -> ""
  | [x] -> string2_of x
  | x :: rest -> string2_of x ^ " " ^ string_of rest
and string2_of = function
  | Int x  -> string_of_int x
  | Id x  -> x
  | List x  -> "(" ^ string_of x ^ ")"
  | Cons (x, y) ->
     (match y with 
       Cons _ ->  "(" ^ string2_of x ^ " " ^ string2_of y ^ ")"
     | _ -> "(" ^ string2_of x ^ " . " ^ string2_of y ^ ")"
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

let rec body_to_exp2 = function
    [] -> UnspecifiedExp
  | x -> body_to_exp x

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
  | Int _ | Bool _ | Char _ | String _ | Vector _ as a -> SelfEvalExp a
  | Id x  -> VarExp x
  | List x -> parseForm x

   (* sparserが できる限りListにしているのに依存 *)
  | Cons (x, y) ->
       parseForm (to_list x y)


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
             UnquoteSplice _ -> raise (ParseError ("qq splice: " ^ string_of rest))
           | x -> QuasiQuoteExp x
          )
      | qq -> raise (ParseError ("quasiquote: " ^ string_of qq))
      )
  | Id "unquote" :: x | Id "unquote-splicing" :: x ->
           raise (ParseError ("unquote not in qq: " ^ string_of x))
  | Id "if" :: rest ->
     (match rest with
       pred ::conseq :: alt ->
         let altexp = (match alt with
           [x] -> parseExp x
         | [] -> UnspecifiedExp
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
  | Id "or" :: rest ->
      let rec make = function
          [] -> SelfEvalExp (Bool false)
        | [x] -> parseExp x
        | x :: rest ->
           CondVal (parseExp x, make rest) in
      make rest
  | Id "and_" :: rest -> (* derived s expression  *)
     (match rest with
	[] -> SelfEvalExp (Bool true)
        | [x] -> parseExp x
        | x :: rest ->
           (* (if x (and rest) #f) *)
           let a = List ([Id "if"; x; List (Id "and_" :: rest); Bool false]) in
           parseExp a
     )
(*
  | Id "and__" :: rest -> AndExp (parseExplist rest)
  | Id "or__" :: rest -> OrExp (parseExplist rest)
 *)
  | Id "lambda" :: rest ->
     let (ids, varid, exp) = parseLambda rest in
     LambdaExp (ids, varid,exp)

  | Id "cond" :: rest -> parseClauses' rest
  | Id "case" :: rest -> parseCase rest
  | Id "let"   :: rest -> parseLet rest
(*
  | Id "let_"  :: rest -> parseLet_ rest
  | Id "let__" :: rest -> parseLet__ rest
 *)
  | Id "let*" :: rest -> parseLetstar rest
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
  | Id "do" :: rest -> parseDo rest
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
     (match op with
     | Id "llet" | Id "aand" | Id "oor" | Id "ccond" | Id "ddo"
     | Id "bbegin" | Id "ccase" | Id "llet*" | Id "lletrec" ->
         let (Id var) = op in
         (* let a = List.map (fun e -> QuoteExp e) rest in *)
         MacroAppExp (var, rest)
     | _ -> let opp = parseExp op and args = parseExplist rest in
            ApplyExp (opp, args)
     )
  | [] -> raise (ParseError "empty form")

and parseLambda = function
    List idlist :: rest ->
     let ids = parseIdlist idlist in
      (ids, Fixed, parseBodyLetrec rest)
  | Id id :: rest ->
      ([], Vararg id, parseBodyLetrec rest)
  | Cons _ as x :: rest ->
     let (ids, varid) = parseIdCons x in
      (ids, varid, parseBodyLetrec rest)
  | x -> raise (ParseError ("lambda: " ^ string_of x))

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

and parseLetstar = function
  | List binds :: body ->
     parseExp
       (match binds with
          [] -> List (Id "let" :: List [] :: body)
        | List [name; value] :: rest ->
           List [Id "let"; List [List [name; value]]; List (Id "let*" :: List rest :: body)]
        | _ -> raise (ParseError "ler*")
       )
  | _ -> raise (ParseError "let*")



and parseDo = function (* 外側の変数looppを隠してしまう *)
(* testが成立すると, expsを評価してdoを抜ける *)
  | List specs :: test_exps :: commands -> 
     let (vars, inits, steps) = splitSpecs specs
     and test, exps = (match test_exps with 
		| List (test :: exps) -> (parseExp test, parseExplist exps)
		| _ -> raise (ParseError "do: test spec"))
     and cmds = parseExplist commands in
(* (letrec
     ((loop (lambda (vars) (if test (begin exps) (begin commands (loop steps))))))
     (loop inits)
  *)
     let apploop = ApplyExp (VarExp "loopp", steps) in
     let next = body_to_exp (cmds @ [apploop]) in
     let loopbody = IfExp (test, body_to_exp2 exps, next) in
     let loop = LambdaExp (vars, Fixed, loopbody) in
     LetrecExp (["loopp", loop], ApplyExp (VarExp "loopp", inits))
 
  | _ -> raise (ParseError "do ")

and parseDo_ = function
(* testが成立すると, expsを評価してdoを抜ける *)
  | List specs :: test_exps :: commands ->
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
     DoExp (specs, test, body_to_exp2 exps, cmds)
 
  | _ -> raise (ParseError "do ")

and splitSpecs : sexp list ->  (id list * exp list * exp list)  = function
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
    [] -> UnspecifiedExp  (* else節なし  *)
  | List e :: rest ->
      (match e, rest with
        Id "else" :: body,  [] ->
          body_to_exp (List.map parseExp body)
      | Id "else" :: _,     _  ->
          raise (ParseError "else clause is not last")
      | [test; Id "=>"; fn],_  ->
          CondArrow (parseExp test, parseExp fn, parseClauses' rest)
(*
      | test :: [],         [] ->
          parseExp test
 *)
      | test :: [],         _  ->
          CondVal (parseExp test, parseClauses' rest)
      | test :: body,       _  ->
          let v = body_to_exp (List.map parseExp body) in
          IfExp (parseExp test, v, parseClauses' rest)
      | [],                 _ -> raise (ParseError "cond clause")
      )
  | _ -> raise (ParseError "cond clause")


and parseCase' = function (* loop helper が不要 *)
    [] -> raise (ParseError "case no key")
  | key :: [] -> raise (ParseError "case no clauses")  

  | List keyargs :: clauses ->
     let a = List [Id "let";
                   List [List [Id "atom-key"; List keyargs]];
                   List (Id "case" :: Id "atom-key" :: clauses)]
     in parseExp a
  | key :: List x :: rest ->
     parseExp (match x, rest with
      | [Id "else"; Id "=>"; result],       [] -> 
         List [result; key]
      | Id "else" :: results,  [] ->
         List (Id "begin" :: results)

      | [List _ as atoms; Id "=>"; result], _ -> 
         let appmemv = List [Id "memv"; key; List [Id "quote"; atoms]] in
         List [Id "if"; appmemv;
                          List [result; key];
                          List (Id "case" :: key :: rest)]
      | List _ as atoms :: results, _ ->
         let appmemv = List [Id "memv"; key; List [Id "quote"; atoms]] in
         List [Id "if"; appmemv; 
               List (Id "begin" :: results);
               List (Id "case" :: key :: rest)]

      | _,_ -> raise (ParseError "case 1") 
     )

and parseCase = function (* 外側の変数atom-keyを隠してしまう *)
    [] -> raise (ParseError "case no key")
  | key :: [] -> raise (ParseError "case no clauses")
  | key :: clauses ->
     match key with
       List _ ->
       let a = List [Id "let";
                     List [List [Id "atom-key"; key]];
                     List (Id "case" :: Id "atom-key" :: clauses)]
       in parseExp a
     | _ ->
       let key = parseExp key in
       let rec loop = function
           [] ->  UnspecifiedExp
         | List x :: rest ->
            (match x, rest with
             | [Id "else"; Id "=>"; result],       [] -> 
                let a = parseExp result in
                ApplyExp (a, [key])
             | Id "else" :: results,                [] ->
                body_to_exp (List.map parseExp results)
             | [List _ as atoms; Id "=>"; result], _  -> 
                let a = parseExp result 
                and appmemv = ApplyExp (VarExp "memv", [key; QuoteExp atoms]) in
                IfExp (appmemv, ApplyExp (a, [key]), loop rest)
             | List _ as atoms :: results,         _ ->
                let a = List.map parseExp results 
                and appmemv = ApplyExp (VarExp "memv", [key; QuoteExp atoms]) in
                IfExp (appmemv, body_to_exp a, loop rest)
             | _ -> raise (ParseError "case: not else and atoms not list") 
            )
         | _ -> raise (ParseError "case clause not list") in
       loop clauses


and parse_qq level : sexp -> qqexp = function
  | Syntax.List x | Syntax.Vector x ->
     let rec loop level = function
         [] -> Nil
      | [Syntax.Id "unquote" as u; a] ->
         if level = 0 then
           Unquote (parseExp a)
         else
           P (parse_qq (level - 1) u, P (parse_qq (level - 1) a, Nil))
      | [Syntax.Id "quasiquote" as u; a] ->
         P (parse_qq (level + 1) u, P (parse_qq (level + 1) a, Nil))

      | Syntax.Id "unquote" :: _ ->
         raise (ParseError "unquote format")
      | Syntax.Id "unquote-splicing" :: _ ->
         raise (ParseError "unquote-splicing: invalid context")
      | Syntax.Id "quasiquote" :: _ ->
         raise (ParseError "quasiquote format")

      | a :: rest -> P (parse_qq level a, loop level rest) in
     (match x with
(*
      | [Syntax.Id "quasiquote" as u; a] ->
         P (parse_qq (level + 1) u, P (parse_qq (level + 1) a, Empty))
 *)
      | [Syntax.Id "unquote-splicing" as u; a] -> (* listの中にあるべき *)
         if level = 0 then
           UnquoteSplice (parseExp a)
         else
           P (parse_qq (level - 1) u, P (parse_qq (level - 1) a, Nil))
 
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

and parseBodyLetrec sexps : exp =
  let (defs, exp) = parseBody sexps in
  match defs with
    [] -> exp
  | x -> LetrecExp (defs, exp)


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

and parseDefMacro : sexp list -> define = function
  (* (define (var formals) exps *)
  | List (Id var :: formals) :: exps ->
      let ids = parseIdlist formals and body = parseBodyLetrec exps in
      var, MacroExp (ids, Fixed, body)
  | Cons (Id var, x) :: exps ->
      let (ids, varid) = parseIdCons x and body = parseBodyLetrec exps in
      var, MacroExp (ids, varid, body)
  (* (define id ex *)
  | [Id var; List (Id "lambda" :: rest)] ->
     let (ids, varid, exp) = parseLambda rest in
     var, MacroExp (ids, varid, exp)
  | _ -> raise (ParseError "define-macro: invalid form")


and parseBody : sexp list -> body = function
  | [] -> raise (ParseError "parse body: empty body")
  | List (Id "define" :: x) :: rest ->
      let def = parseDef x and (defs, e) = parseBody rest in
      def :: defs, e
  | List (Id "define-macro" :: x) :: rest ->
      let def = parseDefMacro x and (defs, e) = parseBody rest in
      def :: defs, e
  | exl -> let a = List.map parseExp exl in
           [], body_to_exp a



let rec parseDefs = function
    [] -> []
  | List (Id "define" :: x) :: rest ->
      parseDef x  :: parseDefs rest
  | List (Id "define-macro" :: x) :: rest ->
      parseDefMacro x :: parseDefs rest
  | x :: exl -> parseDefs exl



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

