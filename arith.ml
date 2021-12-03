(*
   Programming Homework 4, Problem 5: Completing the ARITH Interpreter
   
   In this problem, you are to complete the ARITH Interpreter so that it is correct
   in the sense formulated in Doggie Bag 10, Theorem 1.1. In particular, you will need
   to complete redx so that it is complete for arithmetic expression form reduction.

   Completed solutions should be an executable OCaml code file, that can be read into
   the OCaml interpreter (ocaml) with the #use directive, for example if you've started
   ocaml in the directory where arith.ml resides you would do:

      # #use "arith.ml";;

   This assignment template as provided is an executable OCaml code file.
 *)

(*
   Abstract Syntax
   ---------------
  
   The expr datatype defines the ASTs for CatML. The mapping from CatML concrete syntax
   to abstract syntax is as follows, in full detail. This mapping is implemented by the
   parser in the online tool at http://ceskalka.w3.uvm.edu/225/catml/catml.php.
 
   [[True]] = Bool(true)
   [[False]] = Bool(false)
   [[n]] = Nat(n)           for any natural number n
   [[e1 + e2]] = Plus([[e1]], [[e2]])
   [[e1 - e2]] = Minus([[e1]], [[e2]])
   [[e1 And e2]] = And([[e1]], [[e2]])
   [[e1 Or e2]] = Or([[e1]], [[e2]])
   [[Not e]] = Not([[e]])
*)
type expr =
     (* boolean expression forms *)
     Bool of bool | And of expr * expr | Or of expr * expr | Not of expr   
     (* arithmetic expression forms *)
   | Nat of int | Plus of expr * expr | Minus of expr * expr | Equal of expr * expr  

(*
  isval : expr -> bool
  in : AST [[e]]
  out : true iff e is a value
*)
let isval e = match e with 
     Nat(_) -> true
   | Bool(_) -> true
   | _ -> false

exception StuckExpr

(* 
   redx : expr -> expr
   in : AST [[e]]
   out : AST [[e']] such that e -> e' in the operational semantics
   side effect : exception NotReducible raised if [[e]] isn't reducible in implementation.
   NOTE : This function is incomplete as given.
*)
let rec redx e = match e with
     Not(Bool(false)) -> Bool(true) 
   | Not(Bool(true)) -> Bool(false)
   | And(Bool(_), Bool(false)) -> Bool(false)
   | And(Bool(true), Bool(true)) -> Bool(true)
   | And(Bool(false), Bool(_)) -> Bool(false)
   | Or(Bool(true), Bool(_)) -> Bool(true)
   | Or(Bool(false), Bool(false)) -> Bool(false)
   | Or(Bool(false), Bool(true)) -> Bool(true)
   | Not(e) -> Not(redx e)
   | And(e1,e2) -> if isval e1 then And(e1, redx e2) else And(redx e1, e2)
   | Or(e1, e2) -> if isval e1 then Or(e1, redx e2) else Or(redx e1, e2)
   | _ -> raise StuckExpr

(*
   redxs : expr -> expr
   in : AST [[e]]
   out : [[v]] such that e ->* v in the operational semantics
*)
let rec redxs e = match e with 
     Bool(b) -> Bool(b)
   | Nat(n) -> Nat(n)  
   | _ -> redxs (redx e)

open Printf;;

(*
  prettyPrint : expr -> string
  in : An expression AST [[e]].
  out : The concrete expression e in string format.
*)
let rec prettyPrint e = match e with
   | Bool true -> "True"
   | Bool false -> "False"
   | Nat n -> sprintf "%i" n
   | And (e1, e2) -> "(" ^ (prettyPrint e1) ^ " And " ^ (prettyPrint e2) ^ ")"
   | Or (e1, e2) -> "(" ^ (prettyPrint e1) ^ " Or " ^ (prettyPrint e2) ^ ")"
   | Not e1 -> "(Not " ^ (prettyPrint e1) ^ ")"
   | Plus (e1, e2) -> "(" ^ (prettyPrint e1) ^ " + " ^ (prettyPrint e2) ^ ")"
   | Minus (e1, e2) -> "(" ^ (prettyPrint e1) ^ " - " ^ (prettyPrint e2) ^ ")"
   | Equal (e1, e2) -> "(" ^ (prettyPrint e1) ^ " = " ^ (prettyPrint e2) ^ ")"

(*
  stepper : expr -> expr
  in : AST [[e]]
  out : [[v]] such that e ->* v in the operational semantics
  side effects : Blocks on keystroke between reductions, prints intermediate 
    expressions (aka the reduction trace) during evaluation 
*)
let rec stepper e =
  (printf "%s" (prettyPrint e); flush stdout; read_line();
   match e with 
     Bool(b) -> Bool(b)
   | Nat(n) -> Nat(n)  
   | _ -> (printf "->\n"; flush stdout; stepper (redx e)))

(*
  tracer : expr -> expr
  in : AST [[e]]
  out : [[v]] such that e ->* v in the operational semantics
  side effects : prints intermediate expressions (aka the reduction trace) during evaluation 
*)
let rec tracer e =
  (printf "%s" (prettyPrint e); flush stdout;
   match e with 
     Bool(b) -> Bool(b)
   | Nat(n) -> Nat(n)  
   | _ -> (printf "\n->\n"; flush stdout; tracer (redx e)))
