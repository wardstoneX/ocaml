let length l = let rec aux l acc = match l with
|[] -> acc
| x::xs -> aux xs (acc+1)
in aux l 0;;

let sum_int l = let rec aux l acc = match l with
| [] -> acc
| x::xs -> aux xs (acc + x)
in aux l 0;; 

let sum_float l = let rec aux l acc = match l with
| [] -> acc
| x::xs -> aux xs (acc +. x)
in aux l 0.0;; 


let prod_int l = let rec aux l acc = match l with
| [] -> acc
| x::xs -> aux xs (acc * x)
in aux l 1;; 

let map l f = let rec aux l f acc = match l with
| [] -> acc
| x::xs -> aux xs f ((f x)::acc)
in aux l f [];;

let rec fold_left f acc = function
| [] -> acc
| x::xs -> fold_left f (f acc x) xs

let filter l p = let rec aux l p acc = match l with
| [] -> acc
| x::xs -> if (p x) then aux xs p (x::acc) else  aux xs p acc
in aux l p [];;

let partition l p = let rec aux l p acc = match l with
| [] -> acc
| x::xs -> if (p x) then aux xs p ((x:: (fst acc)), snd acc)
 else aux xs p (fst acc, (x:: (snd acc)))
in aux l p ([],[]);;
