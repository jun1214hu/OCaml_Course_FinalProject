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
   Return a set of the variable names that are free?? in expression
   exp *)

   (* NEED TO DO : TESTING *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  (* There are no variables *)
  | Num _ | Bool _ | Raise | Unassigned -> SS.empty
  (* A set with only one element *)
  | Var v -> SS.singleton v
  (* Unops have variables only in e*)
  | Unop (u, e) -> free_vars e
  (* Binops have variables in both e1 and e2 *)
  | Binop (b, e1, e2) -> SS.union(free_vars e1) (free_vars e2)
  (* Add all free variables for all conditions *)
  | Conditional (i, t, e) -> SS.union (free_vars e) 
                             (SS.union(free vars i) (free_vars t))
  (* Whatever variable is not defined *)
  | Fun (v, e) -> SS.diff (SS.singleton v) (free_vars e)
  (* Combine variables in e1 and e2 and add in the extra v*)
  | Let (v, e1, e2) -> SS.add v (SS.union (free_vars e1)(free_vars e2))
  (* Combine variables in e1 and e2 and add in the extra v*)
  | Letrec (v, e1, e2) -> SS.add v (SS.union (free_vars e1)(free_vars e2))
  (* find free variables in both expressions *)
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
   ;;
  
(* new_varname : unit -> varid
   Return a fresh variable, constructed with a running counter a la
   gensym. Assumes no variable names use the prefix "var". (Otherwise,
   they might accidentally be the same as a generated variable name.) *)
let new_varname () : varid =
  let count = ref 0 in
    fun () -> 
      let var = "var" ^ string_of_int !count in 
      count := !count + 1;
      var ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst : varid -> expr -> expr -> expr
   Substitute repl for *free* occurrences of var_name in exp *)

   (* HAVE TO TEST THIS OUT *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  match exp with
  | Num _ | Bool _ | Raise | Unassigned -> exp
  | Var v -> if v = var_name then repl else exp
  | Unop (u, e) -> Unop (u, subst var_name repl e)
  | Binop (b, e1, e2) -> 
    Binop (b, subst var_name repl e1, subst var_name repl e2)
  | Conditional (i, t, e) -> Conditional (subst var_name repl i,
                             subst var_name repl t, subst var_name repl e)
  | Fun (v, e) -> if v = var_name then expl
                  else Fun (v, subst var_name repl e)
  | Let (v, e1, e2) -> if v = var_name then 
                         Let (v, subst var_name repl e1, e2)
                       else 
                         Let (v, subst var_name repl e1, subst var_name repl e2)
  | Letrec (v, e1, e2) -> if v = var_name then exp
                          else 
                            Letrec (v, subst var_name repl e1, 
                                    subst var_name repl e2)
  | App (e1, e2) -> App (subst var_name repl e1, subst var_name repl e2)

(*......................................................................
  String representations of expressions
 *)
   
    
(* exp_to_concrete_string : expr -> string
   Returns a concrete syntax string representation of the expr *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  (* variables *)
  | Var v -> v    
  (* integers *)
  | Num n -> string_of_int n       
  (* booleans *)
  | Bool b -> string_of_bool b    
  (* Unary operator *)
  | Unop (u, e) -> "~(" ^ exp_to_concrete_string e ^ ")"
  (* binary operators*)
  | Binop (b, e1, e2) -> "(" ^ exp_to_concrete_string e1 ^      
    (match b with
      | Plus -> "+" ^ exp_to_concrete_string e2 ^ ")"
      | Minus -> "-" ^ exp_to_concrete_string e2 ^ ")"
      | Times -> "*" ^ exp_to_concrete_string e2 ^ ")"
      | Equals -> "=" ^ exp_to_concrete_string e2 ^ ")"
      | LessThan -> "<" ^ exp_to_concrete_string e2 ^ ")")
  | Conditional (i, t, e) -> "If (" ^ exp_to_concrete_string i ^ ")" ^
                             "Then (" ^ exp_to_concrete_string t ^ ")" ^
                              "Else (" ^ exp_to_concrete_string e ^ ")" 
  | Fun (v, e) -> v ^ " = " ^ exp_to_concrete_string e
  | Let (v, e1, e2) -> "Let " ^ v ^ " = " ^ exp_to_concrete_string e1 
                       ^ " in " ^ exp_to_concrete_string e2 
  | Letrec (v, e1, e2) -> "Let rec " ^ v ^ " = " ^ exp_to_concrete_string e1 
                       ^ " in " ^ exp_to_concrete_string e2 
  (* exceptions *) 
  | Raise -> "Raise"                               
  (* (temporarily) unassigned *)
  | Unassigned -> "Unassigned"                           
  (* function applications*)
  | App (e1, e2) -> "Apply " ^ exp_to_concrete_string e1 
                    ^ " to " ^ exp_to_concrete_string e2

(* exp_to_abstract_string : expr -> string
   Returns a string representation of the abstract syntax of the expr *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  (* variables *)
  | Var v -> "Var " ^ v    
  (* integers *)
  | Num n -> "Num " ^ string_of_int n       
  (* booleans *)
  | Bool b -> "Bool " ^ string_of_bool b    
  (* Unary operator *)
  | Unop (u, e) -> "Unop (Negate, " ^ exp_to_abstract_string e ^ ")"
  (* binary operators*)
  | Binop (b, e1, e2) -> "Binop (" ^        
    (match b with
      | Plus -> "Plus (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ "))"
      | Minus -> "Minus (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ "))"
      | Times -> "Times (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ "))"
      | Equals -> "Equals (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ "))"
      | LessThan -> "LessThan (" ^ exp_to_abstract_string e1 ^ ", " 
                         ^ exp_to_abstract_string e2 ^ "))")
  | Conditional (i, t, e) -> "Conditional (" ^ exp_to_abstract_string i ^ ", "
                              ^ exp_to_abstract_string t ^ ", " 
                              ^ exp_to_abstract_string e ^ ")" 
  | Fun (v, e) -> "Fun (, " ^ v ^ ", " ^ exp_to_abstract_string e ^ ")"
  | Let (v, e1, e2) -> "Let (" ^ v ^ ", " ^ exp_to_abstract_string e1 
                       ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  | Letrec (v, e1, e2) -> "Letrec (" ^ v ^ ", " ^ exp_to_abstract_string e1 
                          ^ ", " ^ exp_to_abstract_string e2 ^ ")"
  (* exceptions *) 
  | Raise -> "Raise"                               
  (* (temporarily) unassigned *)
  | Unassigned -> "Unassigned"                           
  (* function applications*)
  | App (e1, e2) -> "App (" ^ exp_to_abstract_string e1 
                    ^ ", " ^ exp_to_abstract_string e2 ^ ")"