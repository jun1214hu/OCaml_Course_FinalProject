(* 
                         CS 51 Final Project
                         MiniML -- Evaluation
                             Spring 2018
*)

(* This module implements a small untyped ML-like language under
   various operational semantics.
 *)
    
open Expr ;;
  
(* Exception for evaluator runtime, generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime, generated by an explicit "raise" construct *)
exception EvalException ;;


(*......................................................................
  Environments and values 
 *)

module type Env_type = sig
    type env
    type value =
      | Val of expr
      | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct
    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    (* Creates an empty environment *)
    let create () : env = []

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp : expr) (env : env) : value = Closure (exp, env)

    (* Looks up the value of a variable in the environment *)
    let lookup (env : env) (varname : varid) : value = 
      match List.find (fun (v, _vref) -> v = varname) env with
      | (vid, vref) -> !vref
      | _ -> raise (EvalError "Not Found") 

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env : env) (varname : varid) (loc : value ref) : env =
      (varname, loc) :: env

    (* Returns a printable string representation of an environment *)
    let rec env_to_string (env : env) : string =
      match env with
      | (vid, vref) :: t -> "(" ^ vid ^ ", ref)"  ^ env_to_string t
      | [] -> "" 


    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    let value_to_string ?(printenvp : bool = true) (v : value) : string =
      match v with
      | Val e -> exp_to_concrete_string e
      | Closure (e, env) -> if printenvp then 
                            (exp_to_concrete_string e) ^ env_to_string env
                            else exp_to_concrete_string e 




  end
;;


(*......................................................................
  Evaluation functions

  Returns a result of type value of evaluating the expression exp
  in the environment env. We've provided an initial implementation
  for a trivial evaluator, which just converts the expression
  unchanged to a value and returns it, along with "stub code" for
  three more evaluators: a substitution model evaluator and dynamic
  and lexical environment model versions.

  Each evaluator is of type expr -> Env.env -> Env.value for
  consistency, though some of the evaluators don't need an
  environment, and some will only return values that are "bare
  values" (that is, not closures). *)

(* The TRIVIAL EVALUATOR, which leaves the expression to be evaluated
   essentially unchanged, just converted to a value for consistency
   with the signature of the evaluators. *)
   
let eval_t (exp : expr) (_env : Env.env) : Env.value =
  (* coerce the expr, unchanged, into a value *)
  Env.Val exp ;;

(* The SUBSTITUTION MODEL evaluator -- to be completed *)
(* simplest bits of the language the to most complex*)


(* TO DO - UNIT TESTS *)
(* no need to look at the environment for the sub evaluation *)
   
let eval_s (exp : expr) (_env : Env.env) : Env.value =
  let rec eval (exp : expr) : expr =
  match exp with
  (* these all evaluate to themselves *)
  | Num _ | Bool _ | Fun _ -> exp
  | Raise -> raise (EvalError "Exception was raised")
  | Unassigned -> raise (EvalError "Unassigned")
  (* if the variable does not have a value *)
  | Var _ -> raise (EvalError "Variable cannot be evaluated")
  | Unop (u, e) ->
    (match u, eval e with
    | Negate, Num n -> Num (n * -1)
    | Negate, _ -> raise (EvalError "Cannot negate a non-number")
    | _ -> raise (EvalError "Invalid unop"))
  | Binop (b, e1, e2) -> 
    (match b, e1, e2 with
    | Plus, Num a, Num b -> Num (a + b)
    | Minus, Num a, Num b -> Num (a - b)
    | Times, Num a, Num b -> Num (a * b)
    | Equals, Num a, Num b -> Bool (a = b)
    (* Equals can also be for other non-number cases *)
    | Equals, Bool a, Bool b -> Bool (a = b)
    | LessThan, Num a, Num b -> Bool (a < b)
    | _ -> raise (EvalError "Invalid binop"))
  | Conditional (i, t, e) -> if i = Bool true then eval t else eval e
  | Let (v, e1, e2) -> eval (subst v (eval e1) e2)
  (* something is wrong here. *)
  | Letrec (v, e1, e2) -> 
      let evaled1 = eval (subst v (Letrec (v, e1, Var v)) e1) 
      in eval (subst v evaled1 e2)
  | App (e1, e2) -> 
      (match eval e1 with
      | Fun (v, e) -> eval (subst v (eval e2) e)
      (* bad redex was the error when I ran it *)
      | _ -> raise (EvalError "bad redex"))
  (* in case there's an off condition *)
  | _ -> raise (EvalError "none of the conditions were met")
in Env.Val (eval exp) ;;
     
(* The DYNAMICALLY-SCOPED ENVIRONMENT MODEL evaluator -- to be
   completed *)

let eval_d (exp : expr) (env : Env.env) : Env.value =
  let rec eval (exp : expr) (env : Env.env) : expr =
  match exp with
  | Num _ | Bool _ | Fun _ -> exp
  | Raise -> raise (EvalError "Exception was raised")
  | Unassigned -> raise (EvalError "Unassigned")
  (* The variable may be defined in the environment *)
  | Var v -> 
      (match Env.lookup env v with
      | Env.Val e -> e
      | _ -> raise (EvalError "Variable is undefined"))
  (* should not be different from sub*)
  | Unop (u, e) ->
    (match u, eval e env with
    | Negate, Num n -> Num (n * -1)
    | Negate, _ -> raise (EvalError "Cannot negate a non-number")
    | _ -> raise (EvalError "Invalid unop"))
  (* should not be different from sub*)
  | Binop (b, e1, e2) -> 
    (match b, e1, e2 with
    | Plus, Num a, Num b -> Num (a + b)
    | Minus, Num a, Num b -> Num (a - b)
    | Times, Num a, Num b -> Num (a * b)
    | Equals, Num a, Num b -> Bool (a = b)
    | Equals, Bool a, Bool b -> Bool (a = b)
    | LessThan, Num a, Num b -> Bool (a < b)
    | _, Num _, Num _ | _, Bool _, Bool _ -> 
        raise (EvalError "Invalid operator")
    | _ -> raise (EvalError "Invalid expressions for binop"))
  (* should not be different from sub*)
  | Conditional (i, t, e) -> if i = Bool true then eval t env else eval e env
  | Let (v, e1, e2) -> 
  (* add the variable to the environment and evaluate against e2 *)
      eval e2 (Env.extend env v (ref (Env.Val (eval e1 env))))
  | Letrec (v, e1, e2) -> 
      (* in the interim, store a special value *)
      let interim_var = ref (Env.Val Unassigned) in
      (* evaluate the definition in this extended environment *)
      (* make sure it's the value not variable name *)
      let new_env = Env.extend env v interim_var in 
      (* evaluate e1 in the new environment *)
      let new_e1 = eval e1 new_env in 
      (* then update to the final environment that e2 will be evaluated against *)
      let final_env = Env.extend new_env v interim_var in
      eval e2 final_env
  | App (e1, e2) -> 
      (match eval e1 env with
      (* must extend the environment again *)
      | Fun (v, e) -> eval e (Env.extend env v (ref (Env.Val (eval e2 env))))
      | _ -> raise (EvalError "bad redex"))
  | _ -> raise (EvalError "none of the conditions were met")
in Env.Val (eval exp env) ;;
       
(* The LEXICALLY-SCOPED ENVIRONMENT MODEL evaluator -- optionally
   completed as (part of) your extension *)
   
let eval_l (exp : expr) (env : Env.env) : Env.value =
 let rec eval (exp : expr) (env : Env.env) : expr =
  match exp with
  | Num _ | Bool _ -> exp
  | Raise -> raise (EvalError "Exception was raised")
  | Unassigned -> raise (EvalError "Unassigned")
  (* The variable may be defined in the environment *)
  | Var v -> 
      (match Env.lookup env v with
      | Env.Val e -> e
      | _ -> raise (EvalError "Variable is undefined"))
  (* should not be different from sub*)
  | Unop (u, e) ->
    (match u, eval e env with
    | Negate, Num n -> Num (n * -1)
    | Negate, _ -> raise (EvalError "Cannot negate a non-number")
    | _ -> raise (EvalError "Invalid unop"))
  (* should not be different from sub*)
  | Binop (b, e1, e2) -> 
    (match b, e1, e2 with
    | Plus, Num a, Num b -> Num (a + b)
    | Minus, Num a, Num b -> Num (a - b)
    | Times, Num a, Num b -> Num (a * b)
    | Equals, Num a, Num b -> Bool (a = b)
    | Equals, Bool a, Bool b -> Bool (a = b)
    | LessThan, Num a, Num b -> Bool (a < b)
    | _, Num _, Num _ | _, Bool _, Bool _ -> 
        raise (EvalError "Invalid operator")
    | _ -> raise (EvalError "Invalid expressions for binop"))
  (* should not be different from sub*)
  | Conditional (i, t, e) -> if i = Bool true then eval t env else eval e env
  | Let (v, e1, e2) -> 
  (* add the variable to the environment and evaluate against e2 *)
      eval e2 (Env.extend env v (ref (Env.Val (eval e1 env))))
  | Letrec (v, e1, e2) -> 
      (* in the interim, store a special value *)
      let interim_var = ref (Env.Val Unassigned) in
      (* evaluate the definition in this extended environment *)
      (* make sure it's the value not variable name *)
      let new_env = Env.extend env v interim_var in 
      (* evaluate e1 in the new environment *)
      let new_e1 = eval e1 new_env in 
      (* then update to the final environment that e2 will be evaluated against *)
      let final_env = Env.extend new_env v interim_var in
      eval e2 final_env
  | App (e1, e2) -> 
      (match Env.Val (eval e1 env) with
      (* must extend the environment again *)
      | Env.Closure (Fun (v, e), close_env) -> 
          eval e (Env.extend close_env v (ref (Env.Val (eval e2 env))))
      | _ -> raise (EvalError "bad redex"))
  (* modify the code so that the evulation returns a closure containing
  the function and the current environment *)
  | Fun (v, e) -> exp
  | _ -> raise (EvalError "none of the conditions were met")
in Env.Val (eval exp env)  ;;

(* Connecting the evaluators to the external world. The REPL in
   miniml.ml uses a call to the single function evaluate defined
   here. Initially, evaluate is the trivial evaluator eval_t. But you
   can define it to use any of the other evaluators as you proceed to
   implement them. (We will directly unit test the four evaluators
   above, not the evaluate function, so it doesn't matter how it's set
   when you submit your solution.) *)
   
let evaluate = eval_l ;;
