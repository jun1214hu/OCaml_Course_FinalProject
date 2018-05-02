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
let b1 = Binop (Plus, Num 5, Num 4) ;;
let b2 = Binop (Minus, Num 9, Var "x") ;;
let b3 = Binop (Times, Num 9, Num 5) ;;
let b4 = Binop (Equals, Num 9, Num 9) ;;
let b5 = Binop (Equals, Bool true, Bool true) ;;
let b6 = Binop (LessThan, Num 9, Num 5) ;;
let b7 = Binop (LessThan, Num 5, Num 9) ;;
let f = Fun ("x", Binop (Plus, Var "x", Var "x")) ;;
let f2 = Fun ("f", Binop (Plus, Var "x", Var "y")) ;;
let l = Let ("x", Num 5, Binop (Minus, Num 9, Var "x")) ;;
let l2 = Let ("x", Num 5, Binop (Minus, Num 9, Var "y")) ;;
let lr = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), 
	Num(0)), Num(1), Binop(Times, Var("x"), App(Var("f"), 
	Binop(Minus, Var("x"), Num(1)))))), App(Var("f"), Num(4)));;
let lr2 = Letrec("f", Fun("x", Conditional(Binop(Equals, Var("y"), 
	Num(0)), Num(1), Binop(Times, Var("y"), App(Var("f"), 
	Binop(Minus, Var("z"), Num(1)))))), App(Var("f"), Num(4)));;

(* substituted expressions *)
let subvarx = Num 5;
let nosubvarx = Var "x");
let subb2 = Binop (Minus, Num 9, Num 5);
let subf2 = Fun ("f", Binop (Plus, Num 5, Var "y"));
let subl2 = Let ("x", Num 5, Binop (Minus, Num 9, Num 5)) ;;
let sublr2 = Letrec ("f", Fun ("x", Conditional (Binop 
	(Equals, Var "y", Num 0), Num 1, Binop (Times, Var "y", App 
	(Var "f", Binop (Minus, Var "z", Num 1))))), App (Var "f", Num 4));;

(* Environment for env testing *)
let empty_env = Env.create ();;
let x_env = Env.extend (Env.create ()) "x" (ref (Env.Val varx));;
let y_env = Env.extend (Env.create ()) "y" (ref (Env.Val vary));;
let xy_env = Env.extend x_env "y" (ref (Env.Val vary));;

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
assert (free_vars varx = vars_of_list ["x"]);
assert (free_vars num5 = vars_of_list []);
assert (free_vars t = vars_of_list []);
assert (free_vars u5 = vars_of_list []);
assert (free_vars b1 = vars_of_list []);
assert (free_vars b2 = vars_of_list ["x"]);
assert (free_vars f = vars_of_list []);
assert (free_vars f2 = vars_of_list ["y";"x"]);
assert (free_vars l = vars_of_list []);
assert (free_vars l2 = vars_of_list ["y"]);
assert (free_vars lr = vars_of_list []);

(* subst testing *)
assert (subst "x" num5 varx = subvarx);
assert (subst "y" num4 varx = nosubvarx);
assert (subst "x" num5 b2 = subb2);
assert (subst "x" num4 f2 = subf2);
assert (subst "y" num5 l2 = subl2) ;
assert (subst "x" num4 lr2 = sublr2);

(* eval_s testing *)


(* Env module test *)
assert (Env.close varx empty_env = Closure (varx, empty_env));
assert not (Env.lookup empty_env "x" = Env.Val (Var "x"));
assert (Env.lookup x_env "x" = Env.Val (Var "x"));
assert not (Env.lookup y_env "x" = Env.Val (Var "x"));
assert (Env.lookup y_env "y" = Env.Val (War "y"));;
assert (Env.env_to_string xy_env = "(x, ref)(y, ref)")
assert (Env.env_to_string empty_env = "")


(* eval_d testing *)

(* EXTENSION TESTING *)

;;