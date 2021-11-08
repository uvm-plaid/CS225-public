let rec double_all l = 
	 match l with
	    [] -> []
	  | x::xs -> (2 * x) :: (double_all xs);;
    
let rec float_all l = 
	 match l with
	    [] -> []
	  | x::xs -> (float x) :: (float_all xs);;
    
let rec map f l = 
	 match l with
	    [] -> []
	  | x::xs -> (f x) :: (map f xs);;
    
map (fun x -> 2 * x) [1;2;3;4];;

map (fun x -> float x) [1;2;3;4];;

let double_all = map (fun x -> 2 * x);;

let float_all = map float;;

let graph = [(1.1,3.7);(6.2,9.4);(5.5,3.8)];;

let xcoords l = map (fun (x,y) -> y) l;;

type student = { name : string; email : string; gpa : float};;

let bob = { name = "bob"; email = "bob@uvm.edu"; gpa = 3.2};;
    
type rt = {a: int; b: bool};;

let rv = { a = 1 + 2; b = not true };;

type ('a,'b) pair = { first: 'a; second: 'b; };;

let p = { first = 5; second = (fun x -> x + 1 ) };;

type volume = Liters of float | Gallons of float;;

let convert v =
  match v with
     Liters n -> Gallons (n *. 0.264)
   | Gallons n -> Liters (n *. 3.785)     
   
type 'a option = None | Some of 'a

let safediv num denom = if denom = 0 then None else (Some (num / denom))

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree;;

Node(Node(Leaf,1,Leaf), 4, Node(Node(Leaf,2,Leaf),1,Leaf));;

Node(Node(Leaf,'a',Leaf), 'b', Node(Node(Leaf,'d',Leaf),'r',Leaf));;

let bst = Node(Node(Leaf,1,Leaf), 4, Node(Node(Leaf,5,Leaf),7,Node(Leaf,9,Node(Leaf,10,Leaf))));;   

let rec inorder t = match t with
    Leaf -> []
  | Node(tl,x,tr) -> (inorder tl) @ [x] @ (inorder tr);;

let rec treemap f t =
      match t with 
         Leaf -> Leaf
       | Node(tl,x,tr) -> Node(treemap f tl, (f x), treemap f tr);;
