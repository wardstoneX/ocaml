let length l = let rec aux l acc = match l with
|[] -> acc
| x::xs -> aux xs (acc+1)
in aux l 0;;