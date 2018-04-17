(* 
                         CS 51 Final Project
                        MiniML -- Expressions
                             Spring 2018
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;
      
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;
  
(*......................................................................
  Manipulation of variable names (varids)
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars :  varidset -> varidset -> bool
   Test to see if two sets of variables have the same elements (for
   testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list : string list -> varidset
   Generate a set of variable names from a list of strings (for
   testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars : expr -> varidset
   Return a set of the variable names that are free in expression
   exp *)
let free_vars (exp : expr) : varidset =
  failwith "free_vars not implemented" ;;
  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for free occurrences of var_name in exp *)
let subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  failwith "subst not implemented" ;;

(*......................................................................
  String representations of expressions
 *)
   
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let exp_to_concrete_string (exp : expr) : string =
  failwith "exp_to_concrete_string not implemented" ;;

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var v -> "Var %s" ^ v    (* variables *)
  | Num n -> "Num " ^ string_of_int n       (* integers *)
  | Bool b -> "Bool " ^ string_of_bool b    (* booleans *)
  | Unop (u, e) -> "Unop (Negate, " ^ exp_to_abstract_string e ")"
  | Binop (b, e1, e2) -> "Binop (" ^        (* binary operators*)
    (match b with
      | Plus -> "Plus (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ ")"
      | Minus -> "Minus (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ ")"
      | Times -> "Times (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ ")"
      | Equals -> "Equals (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ ")"
      | LessThan -> "LessThan (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ ")")
  | Conditional (i, t, e) -> "Conditional (" ^ exp_to_abstract_string i ^ ", "
    ^ exp_to_abstract_string t ^ ", " ^ exp_to_abstract_string e ^ ")" 
  | Fun (v, e) -> "Fun (" ^ v ^ ", " exp_to_abstract_string e ^ ")" 
  | Let (v, e1, e2) -> "Let (" ^ v ^ ", " ^ exp_to_abstract_string e1 
    ^ ", " ^ exp_to_abstract_string e2 ^ ")"           (* local naming *)
  | Letrec (v, e1, e2) -> "Letrec (" ^ v ^ ", " ^ exp_to_abstract_string e1 
    ^ ", " ^ exp_to_abstract_string e2 ^ ")"       (* recursive local naming *)
  | Raise -> "Raise"                               (* exceptions *)
  | Unassigned -> "Unassigned"                           (* (temporarily) unassigned *)
  | App (e1, e2) -> "App (" ^ exp_to_abstract_string e1 
    ^ ", " ^ exp_to_abstract_string e2 ^ ")"                 (* function applications *)