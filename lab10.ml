https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
(*
This is the notes from lab 10.

In lab10 went over a "clever" solution to part 2 that very few people attempted, where Begin ... End regions are stored in a single Block constructor.
The primary trade off we make with this aproach is, runing is easier, and parsing is harder.
I chose this solution to present becuase I suspect it will be helpful for part 3.

There is a complete solution to part 2 availible in the resources section under piazza: https://piazza.com/bu/fall2019/cs320/resources
That solution gives an implementation more in line with the lecture slides, and the recomendations we made on piazza.
*)

(* 
recall how Begin..End is specified

for environments


	a = 1
	Begin
				a =? 1
	a = 2
				a =? 2
	End
				a = ?1


where a=1 is a shorthand for 
PushI (I 1)
PushN (N "a")
Bind

begin end blocks can see prevous assignments but those assignments are "forgotten" when outside of the block


for Stacks

	PushI (I 1)
	PushI (I 1)
	Begin
	Add			will push an error onto the stack
	PushI (I 2)
	End			return to the old stack with the final value of PushI (I 2) at the top 
	Add         add 2 and 1
	
begin and end blocks operate in an empty stack
*)


(* things that goes on a stack *)
type stackVal = 
    I of int 
  | S of string 
  | N of string
  | B of bool 
  | U
  | E
  
(* But we will also use this for binding variables, 
   as long as we remember variables cannot be bound to errors, or directly to names *)
  

(* well formed instructions *)
type command = PushI of stackVal 
             | PushS of stackVal 
             | PushN of stackVal 
             | PushB of stackVal
             | Push of stackVal
             | Add | Sub | Mul | Div | Rem | Neg
             | Concat
             | And | Or | Not
             | Equal | LessThan
             | If
             | Pop
             | Swap
             | Block of command list (* handle blocks together *)
             | Bind
             | Quit


(* If we limit our interactions with the environment to these helper functions, we can safely change how we encode the environment without changing much other code *)
type env = (string * stackVal) list
let insert (s:string)  (sv : stackVal) (env: env) : env = (s,sv)::env

let rec fetch (name :string)  (env: (string * stackVal) list) : stackVal option = 
    match env with
      | (name' , v) :: rest -> if name = name' then Some v else fetch name rest
      | []                  -> None
      
let empEnv = []


let rec run (commands : command list) (stack: stackVal list) (env: env) : stackVal list = 
  (* if stackVal is a variable what  does it resolve to in the current environment *)
  let res (sv : stackVal) : stackVal = 
    match sv with 
      | N n -> (match fetch n env with  
                  | Some n' -> n' 
                  | None    -> N n)
      | sv -> sv
  in let bad rest : stackVal list  = run rest (E :: stack) env (* everything fails in the same way*)
  in match (commands , stack)  with
  | (Quit        :: _   , _         ) -> stack
  | ([]                 , _         ) -> stack
  
  | (PushI (I i) :: rest, _         ) -> run rest (I i :: stack) env
  
  | (PushS (S s) :: rest, _         ) -> run rest (S s :: stack) env
  
  | (PushN (N n) :: rest, _         ) -> run rest (N n :: stack) env
  
  | (PushB (B b) :: rest, _         ) -> run rest (B b :: stack) env
  
  | (Push U      :: rest, _         ) -> run rest (U :: stack) env
  | (Push E      :: rest, _         ) -> run rest (E :: stack) env
  
  | (Add         :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (I i, I j) -> run rest (I (i+j) :: s') env 
                                          | _ -> bad rest)
  
  | (Sub         :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (I i, I j) -> run rest (I (i-j) :: s') env 
                                          | _ -> bad rest)
  
  | (Mul         :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (I i, I j) -> run rest (I (i*j) :: s') env 
                                          | _ -> bad rest)
  
  | (Div         :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (I i, I 0) -> bad rest
                                          | (I i, I j) -> run rest (I (i/j) :: s') env 
                                          | _ -> bad rest)
  
  | (Rem         :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (I i, I 0) -> bad rest
                                          | (I i, I j) -> run rest (I (i mod j) :: s') env 
                                          | _ -> bad rest)
  
  | (Neg         :: rest, x :: s'   ) -> (match (res x) with 
                                          | (I i) -> run rest (I (-i) :: s') env 
                                          | _ -> bad rest)

  | (Concat      :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (S i, S j) -> run rest (S (i ^ j) :: s') env 
                                          | _ -> bad rest)
  
  | (And         :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (B i, B j) -> run rest (B (i && j) :: s') env 
                                          | _ -> bad rest)
  
  | (Or          :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (B i, B j) -> run rest (B (i || j) :: s') env 
                                          | _ -> bad rest)
  
  | (Not         :: rest, x :: s'   ) -> (match (res x) with 
                                          | (B i) -> run rest (B (not i) :: s') env 
                                          | _ -> bad rest)
  
  | (Equal       :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (I i, I j) -> run rest (B (i = j) :: s') env 
                                          | _ -> bad rest)
  | (LessThan    :: rest, x ::y ::s') -> (match (res x, res y) with 
                                          | (I i, I j) -> run rest (B (i < j) :: s') env 
                                          | _ -> bad rest)
                                             
  | (If          :: rest,x::y::z::s') -> (match res z with 
                                          | B true  -> run rest (y :: s') env 
                                          | B false -> run rest (x :: s') env 
                                          | _ -> bad rest)
  
  | (Pop         :: rest, _ :: s'   ) -> run rest s' env
  
  | (Swap        :: rest, x ::y ::s') -> run rest (y::x::s') env
  
  
  | (Bind        :: rest, N n::x::s') -> (match res x with
                                          | E   -> bad rest
                                          | N _ -> bad rest (* if the variable is unound we get an error *)
                                          | sv  -> run rest s' (insert n sv env))
  
  | (Block ls    :: rest, s'        ) -> let (top :: _) = run ls [] env (* an exception will be thown if the resulting stack is empty *)
                                         in  run rest (top :: s') env

  
  | (_           :: rest, _         ) -> bad rest


  
  
  
(* remember to test! *)
let e2 = run [PushI (I 1); PushI (I 1); Add] [] empEnv
let e3 = run [PushN (N "x"); PushI (I 1); Add] [] (insert "x" (I 7) empEnv)
let e4 = run [PushN (N "x"); PushI (I 1); Add] [] empEnv
let e5 = run [PushI (I 500); PushI (I 2); Mul; PushI (I 2);  Div] [] empEnv
let e6 = run [PushS (S "world!"); PushS (S "hello "); Concat] [] empEnv
let e7 = run [PushI (I 7); PushI (I 8); LessThan] [] empEnv
let e8 = run [PushI (I 7); PushI (I 7); Equal] [] empEnv
let e9 = run [PushI (I 13); PushN (N "a"); Bind; PushI (I 3); PushN (N "name1"); Bind;  PushN (N "a"); PushN (N "name1"); Add] [] empEnv
let e10 = run [PushB (B true); PushS (S "o"); PushS (S "j"); If] [] empEnv
(* ... *)


(* writing *)
let to_string (s : stackVal) : string = 
  match s with
  | I i -> string_of_int i 
  | S s  -> s
  | N n -> n
  | B b -> "<" ^ string_of_bool b ^ ">"
  | U   -> "<unit>"
  | E   -> "<error>"
  


(* parser combinators over exploded strings *)
let explode (s:string) : char list =
  let rec expl i l =
    if i < 0 
    then l 
    else expl (i - 1) (String.get s i :: l)
  in expl (String.length s - 1) []

let implode (cl:char list) : string = 
  String.concat "" (List.map (String.make 1) cl)

 
let is_alpha (c:char): bool = 
  (Char.code 'a' <= Char.code c && Char.code c <= Char.code 'z')
  || (Char.code 'A' <= Char.code c && Char.code c <= Char.code 'Z')

let is_digit (c:char): bool = 
 Char.code '0' <= Char.code c && Char.code c <= Char.code '9'

let rec take_while' (p: 'a -> bool) (es : 'a list) : ('a list) * ('a list) = 
  match es with
  | []      -> ([],[])
  | x :: xs -> if p x then let (chars, rest) = take_while' p xs in  (x :: chars, rest) else ([], x :: xs)

let take_while (p:char -> bool) (s:string) : string * string = 
  let (echars, erest) = take_while' p (explode s) 
  in (implode echars, implode erest)


let parse_int (s : string) : int option = 
    match int_of_string s with    
    | n -> Some n
    | exception _ -> None

let parse_string (s : string) : string option = 
    if String.length s > 1 && String.get s 0 = '"' && String.get s (String.length s - 1) = '"'
    then  Some (String.sub s 1 (String.length s - 2)) (* this is less restrictive then the spec *)
    else None


let parse_name (s : string) : string option = 
    if String.length s > 0 && ( let c = (String.get s 0) in is_alpha c ||  c = '_')
    then  Some s (* this is less restrictive then the spec *)
    else None
    
    
    
let parse_constant (s:string) : stackVal = 
    let s' = String.trim s in
    match s' with
    | "<true>"  -> B true
    | "<false>" -> B false
    | "<unit>"  -> U
    | _ -> match parse_int s' with
           | Some i -> I i
           | None -> match parse_string s' with
                     | Some s -> S s
                     | None -> match parse_name s' with
                               | Some s -> N s
                               | None -> E



let parse_single_command (s:string) : command = 
    match take_while is_alpha (String.trim s) with
    | ("PushI"   , p) -> PushI (parse_constant p)
    | ("PushS"   , p) -> PushS (parse_constant p)
    | ("PushN"   , p) -> PushN (parse_constant p)
    | ("PushB"   , p) -> PushB (parse_constant p)
    | ("Push"    , p) -> Push (parse_constant p)
    | ("Add"     , _) -> Add
    | ("Sub"     , _) -> Sub
    | ("Mul"     , _) -> Mul
    | ("Div"     , _) -> Div
    | ("Rem"     , _) -> Rem
    | ("Neg"     , _) -> Neg
    | ("Pop"     , _) -> Pop
    | ("Swap"    , _) -> Swap
    | ("Concat"  , _) -> Concat
    | ("And"     , _) -> And
    | ("Or"      , _) -> Or
    | ("Not"     , _) -> Not
    | ("LessThan", _) -> LessThan
    | ("Equal"   , _) -> Equal
    | ("If"      , _) -> If
    | ("Bind"    , _) -> Bind
    | ("Quit"    , _) -> Quit
    (* any unknown commands will result in an exception *)

 
(* parsing becomes harder, though it is doable with a "claver" helper function 
this function had the name parse_block in lab 1 and 2.
*)
 
(* group together everything till you see an "End" return the rest for future work *)
let rec parse_until_end (ls :string list) :  (command list) * (string list)  =  
    match ls with 
    | []              -> ([], [])
    
    | "End" ::     tl ->  ([], tl)
    | "Begin" ::     tl ->  let (cls, rest) = parse_until_end tl
                            in let block = Block cls
                            in let (cls', rest') = parse_until_end rest
                            in (block :: cls', rest')
    
    | h ::     tl         -> let com = parse_single_command h
                             in let (cls, rest) = parse_until_end tl
                             in (com :: cls, rest)
  
let pe1 = parse_until_end ["Pop";"Pop";"Pop";"Pop"]
let pe2 = parse_until_end ["Pop";"Pop";"End";"Pop";"Pop"]
let pe3 = parse_until_end ["Begin";"Pop";"End";"Pop"]
let pe4 = parse_until_end ["Begin";"Pop";"End";"Begin";"Begin";"Pop";"End";"Pop";"End";"Pop"]

(* walk through a recursion *)
let pe51 = parse_until_end ["Neg";"Neg";"End";"Pop";"Pop"] (* ([Neg, Neg] , ["Pop";"Pop"]) *)
let pe52 = parse_until_end [      "Neg";"End";"Pop";"Pop"] (* ([     Neg] , ["Pop";"Pop"]) *)
let pe53 = parse_until_end [            "End";"Pop";"Pop"] (* ([        ] , ["Pop";"Pop"]) *)


let pe6 = parse_until_end ["PushB <true>";"Neg";"PushI 10";"Sub";"Quit"] (* ([PushB (B true); Neg; ...],[]) *)
let pe7 = parse_until_end ["Neg";"Pop"] (* ([Neg, Pop],[]) *)
let pe8 = parse_until_end ["Pop";"Neg";"Pop"] (* ([Pop,Neg, Pop],[]) *)

(* walk through a recursion *)
let pe91 = parse_until_end ["Pop";"Neg";"Pop";"End";"Sub";"Quit"] (* ([Pop,Neg, Pop],["Sub";"Quit"]) *)
let pe92 = parse_until_end [      "Neg";"Pop";"End";"Sub";"Quit"] (* ([    Neg, Pop],["Sub";"Quit"]) *)
let pe93 = parse_until_end [            "Pop";"End";"Sub";"Quit"] (* ([         Pop],["Sub";"Quit"]) *)
let pe94 = parse_until_end [                  "End";"Sub";"Quit"] (* ([            ],["Sub";"Quit"]) *)



let pe10 = parse_until_end [ "Begin";  "Begin";  "Pop" ;  "End";  "End"]


let parse_commands (ls :string list) : command list =
  let (coms, _) = parse_until_end ls 
  in coms
  
let pe20 = parse_commands ["PushB <true>";"Neg";"PushI 10";"Sub";"Quit"]
let pe21 = parse_commands [ "Begin"; "Begin"; "Pop" ; "End"; "End"]

let pe22 = parse_commands [ "Begin"; "Pop" ; "End";] (*  [Block [Pop]] *)

let pe23 = parse_commands [ "Begin"; "Begin"; "Pop" ; "End"; "Begin"; "Pop" ; "End"; "End"] (* [Block [Block [Pop]; Block [Pop]] *)
let pe24 = parse_commands [ "Pop" ; "End"; "Pop" ; "Pop" ] (*  ([Pop], ["Pop" ; "Pop" ] ) *)


(* TODO file IO stuff *)