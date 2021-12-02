(*
  Omega
 
  (Fun x . x x) (Fun x . x x)
*)

let omega =
Appl (
  Function (
    Ident "x",
    Appl (
      Var (
        Ident "x"
      ),
      Var (
        Ident "x"
      )
    )
  ),
  Function (
    Ident "x",
    Appl (
      Var (
        Ident "x"
      ),
      Var (
        Ident "x"
      )
    )
  )
 );;

(*
  The Y Combinator
  (Fun f . (Fun x . f (x x)) (Fun x . f (x x)))
*)

let ycomb = 
Function (
  Ident "f",
  Appl (
    Function (
      Ident "x",
      Appl (
        Var (
          Ident "f"
        ),
        Appl (
          Var (
            Ident "x"
          ),
          Var (
            Ident "x"
          )
        )
      )
    ),
    Function (
      Ident "x",
      Appl (
        Var (
          Ident "f"
        ),
        Appl (
          Var (
            Ident "x"
          ),
          Var (
            Ident "x"
          )
        )
      )
    )
  )
);;

(* 
  Summation using Y
  
  Let ycomb = (Fun f . (Fun x . f (x x)) (Fun x . f (x x))) In 
  Let s = (Fun summate . Fun n . If n = 0 Then 0 Else n + (summate (n - 1))) In
  (ycomb s) 5
*)

let ysumexmp =
Let (
  Ident "ycomb",
  Function (
    Ident "f",
    Appl (
      Function (
        Ident "x",
        Appl (
          Var (
            Ident "f"
          ),
          Appl (
            Var (
              Ident "x"
            ),
            Var (
              Ident "x"
            )
          )
        )
      ),
      Function (
        Ident "x",
        Appl (
          Var (
            Ident "f"
          ),
          Appl (
            Var (
              Ident "x"
            ),
            Var (
              Ident "x"
            )
          )
        )
      )
    )
  ),
  Let (
    Ident "s",
    Function (
      Ident "summate",
      Function (
        Ident "n",
        If (
          Equal (
            Var (
              Ident "n"
            ),
            Nat 0
          ),
          Nat 0,
          Plus (
            Var (
              Ident "n"
            ),
            Appl (
              Var (
                Ident "summate"
              ),
              Minus (
                Var (
                  Ident "n"
                ),
                Nat 1
              )
            )
          )
        )
      )
    ),
    Appl (
      Appl (
        Var (
          Ident "ycomb"
        ),
        Var (
          Ident "s"
        )
      ),
      Nat 5
    )
  )
);;

(* 
  The Z combinator: Eager fixed-point combinator for CatML 
  
  (Fun f . (Fun x . f (Fun v . (x x) v)) (Fun x . f (Fun v . (x x) v)))
*)

let zcomb = Function (
  Ident "f",
  Appl (
    Function (
      Ident "x",
      Appl (
        Var (
          Ident "f"
        ),
        Function (
          Ident "v",
          Appl (
            Appl (
              Var (
                Ident "x"
              ),
              Var (
                Ident "x"
              )
            ),
            Var (
              Ident "v"
            )
          )
        )
      )
    ),
    Function (
      Ident "x",
      Appl (
        Var (
          Ident "f"
        ),
        Function (
          Ident "v",
          Appl (
            Appl (
              Var (
                Ident "x"
              ),
              Var (
                Ident "x"
              )
            ),
            Var (
              Ident "v"
            )
          )
        )
      )
    )
  )
);;

(* 
  Summation using Z 
  
  Let zcomb = (Fun f . (Fun x . f (Fun v . (x x) v)) (Fun x . f (Fun v . (x x) v))) In
  Let s = (Fun summate . Fun n . If n = 0 Then 0 Else n + (summate (n - 1))) In
  (zcomb s) 5
*)

let sumexmp = 
 Let (
  Ident "zcomb",
  Function (
    Ident "f",
    Appl (
      Function (
        Ident "x",
        Appl (
          Var (
            Ident "f"
          ),
          Function (
            Ident "v",
            Appl (
              Appl (
                Var (
                  Ident "x"
                ),
                Var (
                  Ident "x"
                )
              ),
              Var (
                Ident "v"
              )
            )
          )
        )
      ),
      Function (
        Ident "x",
        Appl (
          Var (
            Ident "f"
          ),
          Function (
            Ident "v",
            Appl (
              Appl (
                Var (
                  Ident "x"
                ),
                Var (
                  Ident "x"
                )
              ),
              Var (
                Ident "v"
              )
            )
          )
        )
      )
    )
  ),
  Let (
    Ident "s",
    Function (
      Ident "summate",
      Function (
        Ident "n",
        If (
          Equal (
            Var (
              Ident "n"
            ),
            Nat 0
          ),
          Nat 0,
          Plus (
            Var (
              Ident "n"
            ),
            Appl (
              Var (
                Ident "summate"
              ),
              Minus (
                Var (
                  Ident "n"
                ),
                Nat 1
              )
            )
          )
        )
      )
    ),
    Appl (
      Appl (
        Var (
          Ident "zcomb"
        ),
        Var (
          Ident "s"
        )
      ),
      Nat 5
    )
  )
);;

(* 
 Multiplication as the iteration of addition using Z 
 
 Let zcomb = (Fun f . (Fun x . f (Fun v . (x x) v)) (Fun x . f (Fun v . (x x) v))) In
 Let m = (Fun mult . Fun x . Fun y . If y = 0 Then 0 Else x + ((mult x) (y - 1))) In
 ((zcomb m) 6) 9
*)

let multexmp = 
Let (
  Ident "zcomb",
  Function (
    Ident "f",
    Appl (
      Function (
        Ident "x",
        Appl (
          Var (
            Ident "f"
          ),
          Function (
            Ident "v",
            Appl (
              Appl (
                Var (
                  Ident "x"
                ),
                Var (
                  Ident "x"
                )
              ),
              Var (
                Ident "v"
              )
            )
          )
        )
      ),
      Function (
        Ident "x",
        Appl (
          Var (
            Ident "f"
          ),
          Function (
            Ident "v",
            Appl (
              Appl (
                Var (
                  Ident "x"
                ),
                Var (
                  Ident "x"
                )
              ),
              Var (
                Ident "v"
              )
            )
          )
        )
      )
    )
  ),
  Let (
    Ident "m",
    Function (
      Ident "mult",
      Function (
        Ident "x",
        Function (
          Ident "y",
          If (
            Equal (
              Var (
                Ident "y"
              ),
              Nat 0
            ),
            Nat 0,
            Plus (
              Var (
                Ident "x"
              ),
              Appl (
                Appl (
                  Var (
                    Ident "mult"
                  ),
                  Var (
                    Ident "x"
                  )
                ),
                Minus (
                  Var (
                    Ident "y"
                  ),
                  Nat 1
                )
              )
            )
          )
        )
      )
    ),
    Appl (
      Appl (
        Appl (
          Var (
            Ident "zcomb"
          ),
          Var (
            Ident "m"
          )
        ),
        Nat 6
      ),
      Nat 9
    )
  )
);;

(* 
  Factorial using Z 
  
  Let zcomb = (Fun f . (Fun x . f (Fun v . (x x) v)) (Fun x . f (Fun v . (x x) v))) In
  Let mult = zcomb (Fun mult . Fun x . Fun y . If y = 0 Then 0 Else x + ((mult x) (y - 1))) In
  Let f = (Fun fact . Fun n . If n = 0 Then 1 Else (mult n) (fact (n - 1))) In 
  (zcomb f) 6
*)

let factexmp = 
Let (
  Ident "zcomb",
  Function (
    Ident "f",
    Appl (
      Function (
        Ident "x",
        Appl (
          Var (
            Ident "f"
          ),
          Function (
            Ident "v",
            Appl (
              Appl (
                Var (
                  Ident "x"
                ),
                Var (
                  Ident "x"
                )
              ),
              Var (
                Ident "v"
              )
            )
          )
        )
      ),
      Function (
        Ident "x",
        Appl (
          Var (
            Ident "f"
          ),
          Function (
            Ident "v",
            Appl (
              Appl (
                Var (
                  Ident "x"
                ),
                Var (
                  Ident "x"
                )
              ),
              Var (
                Ident "v"
              )
            )
          )
        )
      )
    )
  ),
  Let (
    Ident "mult",
    Appl (
      Var (
        Ident "zcomb"
      ),
      Function (
        Ident "mult",
        Function (
          Ident "x",
          Function (
            Ident "y",
            If (
              Equal (
                Var (
                  Ident "y"
                ),
                Nat 0
              ),
              Nat 0,
              Plus (
                Var (
                  Ident "x"
                ),
                Appl (
                  Appl (
                    Var (
                      Ident "mult"
                    ),
                    Var (
                      Ident "x"
                    )
                  ),
                  Minus (
                    Var (
                      Ident "y"
                    ),
                    Nat 1
                  )
                )
              )
            )
          )
        )
      )
    ),
    Let (
      Ident "f",
      Function (
        Ident "fact",
        Function (
          Ident "n",
          If (
            Equal (
              Var (
                Ident "n"
              ),
              Nat 0
            ),
            Nat 1,
            Appl (
              Appl (
                Var (
                  Ident "mult"
                ),
                Var (
                  Ident "n"
                )
              ),
              Appl (
                Var (
                  Ident "fact"
                ),
                Minus (
                  Var (
                    Ident "n"
                  ),
                  Nat 1
                )
              )
            )
          )
        )
      ),
      Appl (
        Appl (
          Var (
            Ident "zcomb"
          ),
          Var (
            Ident "f"
          )
        ),
        Nat 6
      )
    )
  )
);;
