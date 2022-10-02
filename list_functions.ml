let length l = let rec aux l acc = match l with
|[] -> acc
| x::xs -> aux xs (acc+1)
in aux l 0;;

let sum_int l = let rec aux l acc = match l with
| [] -> acc
| x::xs -> aux xs (acc + x)
in aux l 0;; 