type bst = Leaf | Node of bst * int * bst;;

let insert n tree = let rec aux_insert n tree = match tree with
  | Leaf -> Node(Leaf,n,Leaf)
  | Node(left,x,right) -> if(n < x) then Node (aux_insert n right,x,left) else Node (right,x,aux_insert n left)
in aux_insert n tree;;

let contains n tree = let rec aux_contains n tree = match tree with
  | Leaf -> false
  | Node(left,x,right) -> if(x == n) then true else
             if (n > x) then aux_contains n right else aux_contains n left
  in aux_contains n tree;;

  let rec height tree = match tree with
  | Leaf -> 0
  | Node(l,_,r) -> 1 + Int.max (height l) (height r);;

  let rec get_max = function
  | Leaf -> failwith "Leaf has no max"
  | Node(_,v,r) -> if r = Leaf then v else get_max r;;

  let rec delete n = function
    | Leaf -> Leaf
    | Node (l, v, r) ->
      if n < v then Node(delete n l,v,r)
      else if n > v then Node(l,v,delete n r)
      else if l = Leaf then r
      else let new_v = get_max l in
      Node(delete new_v l,new_v,r);;

  let rec inorder = function
      | Leaf -> []
      | Node (l, v, r) -> inorder l @ [v] @ inorder r

      let rec preorder = function
      | Leaf -> []
      | Node (l, v, r) -> [v] @ preorder l @  preorder r    

      let rec postorder = function
      | Leaf -> []
      | Node (l, v, r) -> postorder l @ postorder r @ [v]


  let rec mirror = function
      | Leaf -> Leaf   
      | Node(l, v, r) -> Node(mirror r, v, mirror l)
      
      
  let rec length = function
      | Leaf -> 0
      | Node(l,v,r) -> 1 + length l + length r


  let rec sum = function
      | Leaf -> 0
      | Node(l,v,r) -> v + length l + length r;;

  let rec prod = function
    | Leaf -> 0
    | Node(l,v,r) -> v * length l * length r;;
    

  let rec map f = function
      | Leaf -> Leaf
      | Node (l, v , r) -> Node(map f l, f v, map f r)
 
  let rec fold_left f acc = function
      | Leaf -> acc
      | Node(l, v, r) ->
        let left_acc = fold_left f acc l in
        let curr_acc = f left_acc v in
        fold_left f curr_acc r
  
  let filter p = 
      let rec impl acc = function
      | Leaf -> acc
      | Node (l, v, r) ->
          let curr_filtered = if p v then insert v acc else acc in
          let left_filtered = impl curr_filtered l in
          impl left_filtered r

let partition p t = (filter p t, filter(Fun.negate p t))