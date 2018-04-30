open Expr ;;
open Evaluation ;;

(* miniml's string to expression function *)
(* example expr to use *)
let varx = Var "x" ;;
let vary = Var "y" ;;
let num5 = Num 5 ;;
let num9 = Num 9 ;;
let num4 = Num 4 ;;
let t = Bool true ;;
let f = Bool false ;;
let u5 = Unop (Negate, Num 5) ;; 
let u9 = Unop (Negate, Num 9) ;;
let u4 = Unop (Negate, Num 4) ;;
let b1 = Binop (Plus, Num 5, Num 4) ;;
let b2 = Binop (Minus, Num 9, Num 5) ;;
let b3 = Binop (Times, Num 9, Num 5) ;;
let b4 = Binop (Equals, Num 9, Num 9) ;;
let b5 = Binop (Equals, Bool true, Bool true) ;;
let b6 = Binop (LessThan, Num 9, Num 5) ;;
let b7 = Binop (LessThan, Num 5, Num 9) ;;
let f = Fun ("x", Binop (Plus, Var "x", Var "x")) ;;
let l = Let ("x", Num 5, Binop (Minus, Num 9, Var "x")) ;;
let lr = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), 
	Num(0)), Num(1), Binop(Times, Var("x"), App(Var("f"), 
	Binop(Minus, Var("x"), Num(1)))))), App(Var("f"), Num(4)));;


let _ =



(* exp_to_abstract_string testing *)
assert (exp_to_abstract_string varx = "Var(x)") ;
assert (exp_to_abstract_string num5 = "Num(5)") ;
assert (exp_to_abstract_string t = "Bool(true)") ;
assert (exp_to_concrete_string u5 = "Unop (Negate, Num(5))");
assert (exp_to_abstract_string b1 = "Binop (Plus, Num(5), Num(4))");
assert (exp_to_abstract_string f = "Fun(x, Binop (Plus, Var(x), Var(x))))");
assert (exp_to_abstract_string l = "Let (x, Num(5), Binop (Minus, Num(9), Var(x))))");
assert (exp_to_abstract_string lr = "Letrec(f, Fun (x, Conditional, Binop (Equals, Var(x), Num(0))), Num(1),
Binop (Times, Var(x), App (Var(f), Binop (Minus, Var(x), Num(1)))))))), App (Var(f), Num(4)))");


(* exp_to_concrete_string testing *)
assert (exp_to_concrete_string vary = "y") ;
assert (exp_to_concrete_string num5 = "5") ;
assert (exp_to_concrete_string t = "true") ;
assert (exp_to_concrete_string u5 = "~(5)") ;
assert (exp_to_concrete_string b1 = "(5 + 4)");
assert (exp_to_concrete_string f = "fun x -> (x + x)");
assert (exp_to_concrete_string l = "let x = 5 in (9 - x)");
assert (exp_to_concrete_string lr = "let rec f = fun x -> if ((x = 0)) then (1) else ((x * f (x - 1))) in f 4")


(* free_vars testing *)


(* subst testing *)



(* eval_s testing *)

(* Env module test *)

(* eval_d testing *)

(* EXTENSION TESTING *)

;;