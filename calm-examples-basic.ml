(*
  (Fun b . If b Then 1 Else 0)
*)
let boolasnat = Function (Ident("b"), If(Var(Ident("b")), Nat(1), Nat(0)));;

(*
  (Fun b . If b Then 1 Else 0) (True And False)
*)
let banappl = Appl(boolasnat, And(Bool(true), Bool(false)));;

(*
 (Fun x . x + 2)
*)
let plus2 =  Function (Ident("x"), Plus(Var(Ident("x")), Nat(2)));;

(*
 (Fun x . x + 2) 32
 *)
let p2app =  Appl(plus2, Nat(32))

(*
  Let x = 12 In (x - 10)
*)
let elet = Let(Ident("x"), Nat(12), Minus(Var(Ident("x")), Nat(10)))

(*
 Let f = (Fun x . x + 2) In f 32
 *)
let p2let = Let(Ident("f"), plus2, Appl(Var(Ident("f")), Nat(32)))

(*
 (Fun f . (Fun g . (Fun x . f (g x))))
*) 
let compose = 
   Function(Ident("f"), 
    Function(Ident("g"), 
     Function (Ident("x"), Appl(Var(Ident("f")), Appl(Var(Ident("g")), Var(Ident("x")))))));;

(*
 (Fun x . x + 4)
*)
let plus4 =  Function (Ident("x"), Plus(Var(Ident("x")), Nat(4)));;

(*
 Let compose = (Fun f . (Fun g . (Fun x . f (g x)))) In
 Let plus2 = (Fun x . x + 2) In
 Let plus4 = (Fun x . x + 4) In
 compose plus2 plus4
*)
let plus6 = Let(Ident("compose"), compose,
	    Let(Ident("plus2"), plus2,
	    Let(Ident("plus4"), plus4,
	             Appl(Appl(Var(Ident("compose")), Var(Ident("plus2"))),
			      Var(Ident("plus4"))))));;

(*
 Let compose = (Fun f . (Fun g . (Fun x . f (g x)))) In
 Let plus2 = (Fun x . x + 2) In
 Let plus4 = (Fun x . x + 4) In
 compose plus2 plus4 12
*)
let e1 = Let(Ident("compose"), compose,
	 Let(Ident("plus2"), plus2,
	 Let(Ident("plus4"), plus4,
	        Appl(Appl(Appl(Var(Ident("compose")), Var(Ident("plus2"))),
			  Var(Ident("plus4"))), Nat(12)))));;


(*
(*
  (Fun f . (Fun g . (Fun x . f (g x)))) (Fun x . x + 2) (Fun x . x + 4)
*)
let plus6 = Appl(Appl(compose, plus2), plus4);;
(*
  (Fun f . (Fun g . (Fun x . f (g x)))) (Fun x . x + 2) (Fun x . x + 4) 2
*)
let e = Appl(plus6, Nat(2));;
*)

(*
  (Fun x . x x)
*)
let selfref = Function(Ident("x"), Appl(Var(Ident("x")), Var(Ident("x"))));;

(*
  (Fun x . x x)(Fun x . x x)
*)
let omega = Appl(selfref, selfref);;
