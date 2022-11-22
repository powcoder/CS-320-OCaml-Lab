https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
(* add procedures to the robot from lab9 *)


(* Add "procedures" to the value type, add "Run" to the instruction type

have procedures operate the robot, they will take in 1 named parameter, 
when procedures are defined they "remember" the current evironment 
when procedures are run they "forget" the bindings created locally

1st how should this be encoded in data types?
*)

type dir = Left | Right


(* simple syntax.. inputs *)
type exp = I of int | Var of string 

(* instructions *)
type inst = Step of dir | Jump of dir * exp | Set of string * exp
          | DefProc of (string (* name of proc *) *  string (* name of param *) * (inst list))
          | Run of (string (* proc name *) * exp)

;;
(*
if the surface syntax was python like a definition with this syntax:

def foo(x):
  Step Left
  Step Left
  Jump Left x

  
would be encoded like
*)
DefProc ("foo", "x", [Step Left; Step Left; Jump (Left, Var "x")]);;

(*
if the surface syntax was python like

foo(1)
*)
Run ("foo", I 1);;

(* foo(y) *)
Run ("foo", Var "y");;

(* foo(asdas) *)
Run ("foo", Var "asdas");;

(* foo(7) *)
Run ("foo", Var "y");



(*  what's in the evn *)
type value = VI of int 
           | Proc of (string (* param name *) * env * (inst list))  (* this forms a closure with all the information we need to run a function *)


(* If we limit our interactions with the environment to these helper functions, we can safely change how we encode the environment without changing much other code *)
and env = (string * value) list 
let insert (s:string)  (sv : value) (env: env) : env = (s,sv)::env

let rec fetch (name :string)  (env: (string * value) list) : value option = 
    match env with
      | (name' , v) :: rest -> if name = name' then Some v else fetch name rest
      | []                  -> None
      
let empEnv = []
  
let rec runRobot (intructions : inst list) (pos:int) (env : env) : int option = 
    let res (e:exp) = 
	   match e with
	     | Var s -> fetch s env
		 | I i   -> Some (VI i)
	in match intructions with
	  | (Step Right)                             :: rest -> runRobot rest (pos + 1) env
	  | (Step Left )                             :: rest -> runRobot rest (pos - 1) env

	  | (Jump (Right, d))                        :: rest -> (match res d with 
	                                                           | Some (VI d') -> runRobot rest (pos + d') env
															   | _ -> None)
	  | (Jump (Left , d))                        :: rest -> (match res d with  
	                                                           | Some (VI d') -> runRobot rest (pos - d') env
															   | _ -> None)
															   
	  | (Set (s,d))                              :: rest -> (match res d with 
	                                                           | Some v -> runRobot rest pos (insert s v env)
															   | _ -> None)
	  
	  | (DefProc (procName, paramName, instrucs)):: rest -> let proc = Proc (paramName, env, instrucs)
	                                                        in runRobot rest pos (insert procName proc env)
	  
	  | (Run (procName, e))                      :: rest -> (match (res (Var procName), res e) with
	                                                           | (Some (Proc (paramName, env', instrucs)), Some v) -> 
										                           (match runRobot instrucs pos (insert paramName v env') with 
												                      | (Some pos') -> runRobot rest pos' env
	                                                                  | _ -> None)
	                                                           | _ -> None)
	  
	  | []                                               -> Some (pos)

(* review how recursion of the lab9 runRobot works*)

let ex11 = runRobot [Set ("x", I 7); Jump (Right, Var "x")] 0 empEnv
let ex12 = runRobot                 [Jump (Right, Var "x")] 0 (insert "x" (VI 7) empEnv)
let ex12 = runRobot                                      [] 7 (insert "x" (VI 7) empEnv)


let ex21 = runRobot [Set ("x", I 7); Jump (Right, Var "x"); Jump (Right, Var "x")] 0  empEnv
let ex22 = runRobot                 [Jump (Right, Var "x"); Jump (Right, Var "x")] 0  (insert "x" (VI 7) empEnv)
let ex23 = runRobot                                        [Jump (Right, Var "x")] 7  (insert "x" (VI 7) empEnv)
let ex24 = runRobot                                                             [] 14 (insert "x" (VI 7) empEnv)



(* make some tests that excersise the behavior *)

(* defining a procedure does not change the position *)
let e3 = runRobot [Jump (Right, I 7); 
                   Step Left; 
                   DefProc ("ahhh","x", [Step Right;
				                        Step Right]) 
				   ] 0 empEnv (* -> 6 *)
				   
(* have procedures operate the robot  *)
let e4 = runRobot [Step Left; 
                   DefProc ("woo", "x", [Step Right]); 
				   Run ("woo", I 8)
				   ] 0 empEnv (* -> 0 *)
				   
let e5 = runRobot [DefProc ("aproc", "cs", [Step Left]);
                   Run ("aproc", I 1)
				   ] 0 empEnv (* -> -1 *)

(* procedures take in 1 named parameter, that operates as expected *)
let e6 = runRobot [Step Left;
                   DefProc ("woo", "x", [Jump (Right, Var "x")]);
				   Run ("woo", I 8)
			       ] 0 empEnv (* ->  7 *)
				   
let e7 = runRobot [DefProc ("aproc", "cs", [Jump (Left, Var "cs")]); 
                   Run ("aproc", I 6)
				   ]  0 empEnv  (* -> -6 *)
				   
let e8 = runRobot [DefProc ("ahhh","x", [Jump (Right, Var "x");
                                         Step Right]);
                   Run ("ahhh", I 10)
				   ]  0 empEnv (* -> 11 *)


(* when procedures are defined they "remember" the current evironment *)
let e9 = runRobot [Step Left; 
                   Set ("y", I 100); 
				   DefProc ("woo", "x", [Jump (Right, Var "y")]); 
				   Run ("woo", I 8)
				   ] 0 empEnv  (* -> 99 *)
				   
let e10= runRobot [Set ("cs", I 5 ); 
                   DefProc ("aproc", "x", [Jump (Left, Var "cs")]);
                   Set ("cs", I 9999 ); 
                   Run ("aproc", I 6)
				   ] 0 empEnv  (* -> -5 *)
 
let e11= runRobot [Set ("x", I 5555);
                   DefProc ("ahhh","y", [Jump (Right, Var "x");
				                         Step Right]);
                   Set ("x", I 0);
                   Run ("ahhh", I 10)
                   ]  0 empEnv (* -> 5556 *)


(* when procedures are run they "forget" the bindings created locally  *)

let e12= runRobot [Step Left; 
                   Set ("x", I 100); 
				   DefProc ("woo", "x", [Jump (Right, Var "x")]); 
				   Run ("woo", I 8); 
				   Jump (Right, Var "x")
				   ] 0 empEnv  (* -> 107 *)

let e13= runRobot [DefProc ("f","y", [Set ("c", I 5555)]);
                   Set ("c", I 999);
                   Run ("f", I 10);
                   Jump (Right, Var "c")
                   ] 0 empEnv (*  -> 999 *)

(* another general test  *)

let ex14= runRobot [Step Left; 
                    Jump (Right, I 4); 
                    DefProc ("go", "umm", [Step Right;
					                       Jump (Left, Var "umm" )]) ;
                    Run ("go", I 3) ] 0 empEnv 



(* there was an additional part that no lab completed *)

(* Add a "Simulate" instruction: 
simulate takes a name of a procedure, a expression for it's parameter, a binding to inspect when the function is completed and a variable name that will be set to that value.
simulate will not move the robot.

1st how should this be encoded in data types?
2nd make some tests that excersise the behavior
3rd modify the runRobot function
*)

(* this was intended to be a hint for IOfunctions *)

(* one posible solution is to change the definition of env
env = (string * value) list list 

and the 3 helper functions.
then you had a stack of environments that you could manually push to and pull from.
*)


(* another posible solution is to change the type of runRobot 
to 
runRobot : (intructions : inst list) -> (pos:int) ->  (env : env)->  env * (int option )
so it also retuned the required environment
*)