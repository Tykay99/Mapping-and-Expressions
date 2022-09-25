(* 

Name: Ty Kay
Netid: tkay

*)

let map : ('a -> 'b) -> 'a list -> 'b list = List.map

let filter : ('a -> bool) -> 'a list -> 'a list = List.filter

let foldr : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = List.fold_right

let foldl : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b = List.fold_left

(* reduce is equivalent to List.fold_right, 
 * only its args are ordered differently *)
let rec reduce (f:'a -> 'b -> 'b) (u:'b) (xs:'a list) : 'b =
  match xs with
    | [] -> u
    | hd::tl -> f hd (reduce f u tl) 

(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)

let negate (x: int) : int =
  0 - x
  
let negate_all (nums:int list) : int list =
  map negate nums

let _ = assert ((negate_all [1; -2; 0]) = [-1; 2; 0])

(*>* Problem 1.1.b *>*)

(*  sum_rows : Takes a list of int lists (call an internal list a "row").
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input. *)
 
let sum (row:int list) : int =
  foldl (+) 0 row
  
let sum_rows (rows:int list list) : int list =
  map sum rows

let _ = assert (sum_rows [[1;2]; [3;4]] = [3; 7])

(*>* Problem 1.1.c *>*)

(*  num_occurs : Returns the number of times a given number appears in a list. *)

let num_occurs (n:int) (nums:int list) : int =
  let y = (filter (fun x -> x = n) nums) in
    if n <> 0 then sum y / n
    else sum (map (fun x -> x + 1) y)

let _ = assert (num_occurs 4 [1;3;4;5;4] = 2)

(*>* Problem 1.1.d *>*)

(*  super_sum : Sums all of the numbers in a list of int lists *) 

let super_sum (nlists:int list list) : int =
  sum (sum_rows nlists)

let _ = assert (super_sum [[1;2;3];[];[5]] = 11)

(*>* Problem 1.2.a *>*)

(* consec_dedupe : removes consecutive duplicate values from a list.
 * More specifically, consec_dedupe has two arguments: 
 *   eq is a function representing an equivalence relation 
 *   xs is a list of elements.  
 * It returns a list containing the same elements as lst, but without 
 * any consecutive duplicates, where two elements are considered 
 * duplicates if applying eq to them yields true.
 * 
 * Example: consec_dedupe (=) [1; 1; 1; 3; 4; 4; 3] = [1; 3; 4; 3].
 * 
 * Example:
 * 
 * let nocase_eq (s1:string) (s2:string) : bool = (String.uppercase s1)
 * = (String.uppercase s2) 
 * 
 * consec_dedupe nocase_eq ["hi"; "HI"; "bi"] = ["hi"; "bi"]
 * 
 * (When consecutive duplicates are not exactly syntactically equal, as
 * above, it does not matter which of the duplicates are discarded.)
 * 
 *)


let consec_dedupe (eq:'a -> 'a -> bool) (xs:'a list) : 'a list =
  let equal (x: 'a) (xs: 'a list) : 'a list =
    match xs with
    | [] -> x :: xs
    | hd :: tl ->
      if eq x hd then hd :: tl
      else x :: hd :: tl in
  foldr equal xs []
  
let _ = assert (consec_dedupe (=) [1; 1; 1; 3; 4; 4; 3] = [1; 3; 4; 3])

(*>* Problem 1.2.b *>*)

(* prefixes: return a list of all non-empty prefixes of a list, 
 * ordered from shortest to longest.
 * 
 * Example: prefixes [1;2;3;4] = [[1]; [1;2]; [1;2;3]; [1;2;3;4]].
 * 
 * There are no non-empty prefixes of an empty list.
*)

let pre (x: 'a) (xs: 'a list list) : 'a list list =
  match xs with 
  | [] -> [[x]]
  | hd :: tl -> 
    [x] :: (map (fun z -> x :: z) xs)
    
let prefixes (xs: 'a list) : 'a list list =
  reduce pre [] xs

let _ = assert (prefixes [1;2;3;4] = [[1]; [1;2]; [1;2;3]; [1;2;3;4]])

(*>* Problem 1.2.c *>*)
(* flatten : write a function that flattens a list of lists into a single
 * list with all of the elements in the order they appeared in the original 
 * lists
 *
 * flatten [[1;2;3]; []; [0]; [4;5]] = [1;2;3;0;4;5] *)

let cons (x: 'a) (y: 'a list) : 'a list =
  x :: y 
  
let zip (xs: 'a list) (ys: 'a list) : 'a list =
  foldr cons xs ys
  
let flatten (xss:'a list list) : 'a list =
  foldr zip xss []

let _ = assert (flatten [[1;2;3]; []; [0]; [4;5]] = [1;2;3;0;4;5])
