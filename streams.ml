(* streams contain a first element and a thunk that returns the rest of the stream *)
type 'a stream = Stream of 'a * (unit -> 'a stream);;

(* head of a stream *)
let hd (Stream (x, _)) = x;;

(* tail of a stream *)
let tl (Stream (_, t)) = t () (* get the tail by evaluating the thunk *)

(* make a list from the first n elements of a stream *)
let rec take n s = if n <= 0 then [] else hd s :: take (n - 1) (tl s);;

let rec take_tl n s a = if n <= 0 then (a, s) else take_tl (n - 1) (tl s) (a@[hd s]);;

(* an infinite stream of 1's *)
let rec ones = Stream (1, fun () -> ones);;

(* the natural numbers *)
let rec from n = Stream (n, fun () -> from (n + 1));;
let naturals = from 0;;

(* remove elements of a stream that don't satisfy a given predicate *)
let rec filter p s = 
  if p (hd s) then Stream (hd s, fun () -> filter p (tl s)) else filter p (tl s);;
      
(* evens and odds as filtered streams *)      
let evens = filter (fun x -> x mod 2 = 0) naturals;;
let odds = filter (fun x -> x mod 2 <> 0) naturals;;

(* delete multiples of p from a stream *)
let sift k = filter (fun n -> n mod k <> 0);;

(* sieve of Eratosthenes *)
let rec sieve (Stream (k, t)) = Stream (k, fun () -> sieve (sift k (t ())));;

(* primes *)
let primes = sieve (from 2);;
