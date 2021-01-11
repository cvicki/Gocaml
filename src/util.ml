
let combine_t op (u1,u2) (v1,v2) =
  (op u1 v1, op u2 v2)

let string_of_list cast lst =
  let rec str acc = function
    | [] -> acc
    | h :: [] -> 
      if acc = "" then cast h
      else acc ^ ", " ^ (cast h)
    | h :: t -> 
      if acc = "" then str (cast h) t
      else str (acc ^ ", " ^ cast h) t
  in "[" ^ (str "" lst) ^ "]" 

let max_triple3 lst = 
  List.fold_left 
    (fun (a,b,acc) (elt1,elt2,elt3) -> 
       if elt3 > acc then (elt1,elt2,elt3) 
       else (a,b,acc)
    ) (-1,-1,-1) lst 

let quartet_swap arr = 
  let rec swap acc = function
    | (v1, v2, v3, v4) :: t -> swap ((v2, v1, v4, v3) :: acc) t
    | [] -> Array.of_list acc
  in swap [] (Array.to_list arr)

let cartesian lst1 lst2 = 
  List.concat (List.map (fun n1 -> List.map (fun n2 -> (n1, n2)) lst2) lst1)

let time () = int_of_float (Unix.time ())

let list_max = function
  | h :: t -> 
    Some (List.fold_left (fun acc x -> if x > acc then x else acc) h t)
  | [] -> None

let random_string len = 
  let rec gen acc n = 
    if n = 0 then acc
    else 
      let rand = 97 + Random.int 26 in
      let char = Char.chr rand |> Char.escaped in
      gen (acc ^ char) (n - 1)
  in gen "" len