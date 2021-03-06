(*
   Homework 5: Completing the CatML interpreter.
   
   DIRECTIONS: In this OCaml code file you will find the beginning of various functions 
   with COMPLETE ME tags in the comments, that you must complete to obtain a *correct*
   interpreter for the CatML language.
   
   Both the tracer and stepper functions (with pretty printing) have been completed
   for you and can be used for testing examples as you work on the assignment. Example
   expressions for testing can be found in the course github repository. The online
   interpreter at http://ceskalka.w3.uvm.edu/225/catml/catml.php can be used to easily 
   generate ASTs for additional examples. You should submit this file once completed. 
   Your submission must be executable OCaml code.
   
   GRADING: Your code will be graded for correctness (in the sense of Lecture Notes 10, 
   Theorem 1.1), by both direct review of code and evaluation of your function definitions 
   on suites of test cases. The breakdown will be as follows:
   
   100 total points
   40 points for syntax and type correctness with a baseline of effort (i.e., you worked
      on a solution which is valid OCaml code. Note that just handing in this file as given 
      does not count as a baseline of effort).
   6 points for style (formatting, comments, elegance).
   54 points for formal correctness of definitions as measured by test cases: 
     - 10 points for expression closure (closed).
     - 44 points for reduction (redx, subst, isval).
     
   Test cases will be of low to high complexity, from simple arithmetic expressions to recursive 
   function definitions (using both fixpoint combinators and the Fix construct). **Only Graduate 
   Students will be tested on evaluation of structured data, i.e., pairs**. In all cases, evaluation 
   on test cases provided in the CS225-public repo will be a great indicator of your progress.
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
   [[x]] = Var(Ident("x"))       for any variable x
   [[e1 + e2]] = Plus([[e1]], [[e2]])
   [[e1 - e2]] = Minus([[e1]], [[e2]])
   [[e1 And e2]] = And([[e1]], [[e2]])
   [[e1 Or e2]] = Or([[e1]], [[e2]])
   [[Not e]] = Not([[e]])
   [[(e1, e2)]] = Pair([[e1]], [[e2]])
   [[Fst(e)]] = Fst([[e]])
   [[Snd(e)]] = Snd([[e]])
   [[e1 e2]] = Appl([[e1]], [[e2]])
   [[Let x = e1 in e2]] = Let(Ident("x"), [[e1]], [[e2]])
   [[(Fun x . e)]] = Fun(Ident("x"), [[e]])
   [[(Fix z . x . e)]] = Fix(Ident("z"), Ident("x"), [[e]])
*)

type ident = Ident of string

type expr =
     (* boolean expression forms *)
     Bool of bool | And of expr * expr | Or of expr * expr | Not of expr   
     (* arithmetic expression forms *)
   | Nat of int | Plus of expr * expr | Minus of expr * expr | Equal of expr * expr  
     (* functional expression forms *)
   | Function of ident * expr | Appl of expr * expr | Var of ident
     (* pairs *)
   | Pair of expr * expr | Fst of expr | Snd of expr
     (* other forms *)
   | If of expr * expr * expr | Let of ident * expr * expr | Fix of ident * ident * expr

exception AssignmentIncomplete

(*
   Closed expression check
   ------------------------
   Since reduction is defined only on closed expressions, we need to implement
   a check to ensure that an input expression is closed. Since closure is preserved
   by reduction, this check can be performed once statically on source code,
   as in tracer and stepper below.
   Note: List.mem can be used to check whether a given value occurs in a list.
   
   closed : expr -> ident list -> bool
   in : an expression e and an identifier list ilist
   out : true iff e is closed, assuming every element of ilist is 
         a bound variable
*)
let rec closed e ident_list = raise AssignmentIncomplete (* COMPLETE ME *)

(*
   Substitution
   ------------
   We implement substitution as defined symbolically, to obtain a substution-based
   semantics in the interpreter.
  
   subst : expr -> expr -> ident -> expr
   in : expression e1, expression e2, identifier id
   out : e1[e2/id] 
*)
let rec subst e1 e2 id = raise AssignmentIncomplete (* COMPLETE ME *)

(*
   Value predicate
   ---------------
   Checking whether a given expression is a value is critical for the operational 
   semantics. 
   isval : expr -> bool
   in : expression e
   out : true iff e is a value
*)
let rec isval e = match e with 
     Nat(_) -> true
   | Bool(_) -> true
   | _ -> raise AssignmentIncomplete (* COMPLETE ME *)

exception StuckExpr

(*
   Single step reduction
   ---------------------
   Single (aka small) step reduction is the heart of the operational semantics. Pattern
   matching allows a tight connection with the symbolic definition of the semantics.
   
   redx : expr -> expr
   in : AST [[e]]
   out : AST [[e']] such that e -> e' in the operational semantics
   side effect : exception NotReducible raised if [[e]] isn't reducible in implementation.
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
   | _ -> raise AssignmentIncomplete (* COMPLETE ME *)

(*
   Multistep reduction
   -------------------
   redxs : expr -> expr
   in : AST [[e]]
   out : [[v]] such that e ->* v in the operational semantics
*)
let rec redxs e = if isval e then e else redxs (redx e)

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
   | Var(Ident(x)) -> x
   | And (e1, e2) -> "(" ^ (prettyPrint e1) ^ " And " ^ (prettyPrint e2) ^ ")"
   | Or (e1, e2) -> "(" ^ (prettyPrint e1) ^ " Or " ^ (prettyPrint e2) ^ ")"
   | Not e1 -> "(Not " ^ (prettyPrint e1) ^ ")"
   | Plus (e1, e2) -> "(" ^ (prettyPrint e1) ^ " + " ^ (prettyPrint e2) ^ ")"
   | Minus (e1, e2) -> "(" ^ (prettyPrint e1) ^ " - " ^ (prettyPrint e2) ^ ")"
   | Equal (e1, e2) -> "(" ^ (prettyPrint e1) ^ " = " ^ (prettyPrint e2) ^ ")"
   | If(e1, e2, e3) -> "If " ^ (prettyPrint e1) ^ 
                       " Then " ^ (prettyPrint e2) ^
                       " Else " ^ (prettyPrint e3)
   | Function(Ident(x), e) -> "(Fun " ^ x ^ " . " ^ (prettyPrint e) ^ ")"
   | Fix(Ident(z), Ident(x), e) -> "(Fix " ^ z ^ " . " ^ x ^ " . " ^ (prettyPrint e) ^ ")"
   | Let(Ident(x), e1, e2) -> "Let " ^ x ^ " = " ^ (prettyPrint e1) ^ " In\n" ^ (prettyPrint e2)
   | Appl(e1, e2) -> (prettyPrint e1) ^ " " ^ (prettyPrint e2)
   | Pair(e1, e2) -> "(" ^ (prettyPrint e1) ^ ", " ^ (prettyPrint e2) ^ ")"
   | Fst(e1) -> 
      (match e1 with Pair(_) -> "Fst" ^  (prettyPrint e1) 
                  | _ ->  "Fst(" ^  (prettyPrint e1) ^ ")")
   | Snd(e1) -> 
      (match e1 with Pair(_) -> "Snd" ^  (prettyPrint e1) 
                  | _ ->  "Snd(" ^  (prettyPrint e1) ^ ")")


exception NotClosed
(*
  stepper : expr -> expr
  in : AST [[e]]
  out : [[v]] such that e ->* v in the operational semantics
  side effects : Blocks on keystroke between reductions, prints intermediate 
    expressions (aka the reduction trace) during evaluation 
*)
let stepper e = if not (closed e []) then raise NotClosed else
 let rec step e =
   (printf "%s" (prettyPrint e); flush stdout; read_line();
    if isval e then e else (printf "\n->\n"; flush stdout; step (redx e))) in step e
				   
(*
  tracer : expr -> expr
  in : AST [[e]]
  out : [[v]] such that e ->* v in the operational semantics
  side effects : prints intermediate expressions (aka the reduction trace) during evaluation 
*)
let rec tracer e = if not (closed e []) then raise NotClosed else
 let rec trace e =
    (printf "%s" (prettyPrint e); flush stdout;
     if isval e then e else (printf "\n->\n"; flush stdout; trace (redx e))) in trace e

