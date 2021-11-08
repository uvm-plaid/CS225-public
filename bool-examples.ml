(* True Or Not False *)

let e1 = 
Or (
  Bool true,
  Not (
    Bool false
  )
);;


(* Not True Or Not True And Not False *)

let e2 = 
Or (
  Not (
    Bool true
  ),
  And (
    Not (
      Bool true
    ),
    Not (
      Bool false
    )
  )
);;

