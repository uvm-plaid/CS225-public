let rec fibnum n = match n with
    0 -> 0
  | 1 -> 1 
  | _ -> fibnum (n-1) + fibnum (n-2);;
 
let rec fibnum_mem n =
  let tab = Array.make (n+1) 0 in 
  let rec fm n = match n with
    0 -> 0
  | 1 -> 1 
  | _ -> if tab.(n) > 0 then tab.(n) else let fn = fm (n-1) + fm (n-2) in (tab.(n) <- fn; fn)
in fm n;;

exception Negative;;

let rec fact n = 
  if n < 0 then raise Negative else 
  match n with
     0 -> 1
   | n -> n * fact (n-1);;
   
type 'a option = None | Some of 'a;;

let fact_option n = try Some(fact n) with Negative -> None;;

let rec for_to_do j k f = 
  	if j > k then () else (f j; for_to_do (j+1) k f)

let fact n =
	let soln = ref 1 in 
  	(for_to_do 1 n (fun i -> soln := (!soln * i)); 
   	 !soln)
