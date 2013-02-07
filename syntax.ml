
(* abstract syntax *)


type sexp =
    Int of int
  | Bool of bool
  | Char of char
  | String of string
  | Id of string
  | List of sexp list  
