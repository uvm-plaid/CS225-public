let f0 = fun x -> (x,x) in 
let f1 = fun y -> f0(f0(y)) in 
f1(fun z -> z);;

let f0 = fun x -> (x,x) in 
let f1 = fun y -> f0(f0(y)) in 
let f2 = fun y -> f1(f1(y)) in
f2(fun z -> z);;

let f0 = fun x -> (x,x) in 
let f1 = fun y -> f0(f0(y)) in 
let f2 = fun y -> f1(f1(y)) in
let f3 = fun y -> f2(f2(y)) in
f3(fun z -> z);;

let f0 = fun x -> (x,x) in 
let f1 = fun y -> f0(f0(y)) in 
let f2 = fun y -> f1(f1(y)) in
let f3 = fun y -> f2(f2(y)) in
let f4 = fun y -> f3(f3(y)) in
f4(fun z -> z);;

let f0 = fun x -> (x,x) in 
let f1 = fun y -> f0(f0(y)) in 
let f2 = fun y -> f1(f1(y)) in
let f3 = fun y -> f2(f2(y)) in
let f4 = fun y -> f3(f3(y)) in
let f5 = fun y -> f4(f4(y)) in
f5(fun z -> z);;
