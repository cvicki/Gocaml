(** Util is a compilation of helper functions useful in the implementation of 
    GOcaml, but with very general applications that may be useful in other 
    scenarios. *)

(** [combine_t op t1 t2] combines the elements of each tuple according to 
    [op]. *)
val combine_t : ('a -> 'b -> 'c) -> 'a * 'a -> 'b * 'b -> 'c * 'c

(** [string_of_list cast lst] concatenates all the elements in [lst] to a single 
    string, with each element cast to a string according to [cast]. *)
val string_of_list : ('a -> string) -> 'a list -> string

(** [max_triple3 lst] finds the largest positive third element of a triple in 
    [lst] and returns the entire triple. 
    Requires all third elements be natural numbers. *)
val max_triple3 : (int * int * int) list -> int * int * int

(** [quartet_swap a] swaps the 1st with the 2nd and the 3rd with the 4th element
    in each quartet. *)
val quartet_swap : ('a * 'b * 'c * 'd) array -> ('b * 'a * 'd * 'c) array

(** [cartesian lst1 lst2] takes the cartesian product of [lst1] with [lst2]. *)
val cartesian : 'a list -> 'b list -> ('a * 'b) list

(** [time] is the current Unix time as an integer. *)
val time : unit -> int

(** [list_max lst] is the maximum element in [lst]. *)
val list_max : 'a list -> 'a option

(** [random_string len] is a random lowercase string of lenght [len]. *)
val random_string : int -> string
