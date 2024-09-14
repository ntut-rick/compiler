(* Exercise 1: Number warm-up *)
(* (a) fact such that fact n returns the factorial of the positive integer n. *)
let rec fact n = 
  match n with
  | 0 -> 1
  | n -> n * fact (n-1)

let _ = assert (fact 4 = 24)
let _ = assert (fact 5 = 120)

(* (b) nb_bit_pos such that bn_bit_pos n returns the number of bits equal to 1
       in the binary representation of the positive integer n *)
let rec nb_bit_pos n =
  match n with
  | 0 -> 0
  | n -> n land 1 + nb_bit_pos (n lsr 1)

let _ = assert (nb_bit_pos 10 = 2)
let _ = assert (nb_bit_pos 11 = 3)

(* ------------------------------------------------------------------------------ *)

(* Exercise 2: Fibonacci *)
(* Here is a naive writing of a function calculating the terms of
   the Fibonacci sequence *)
let rec fibo n =
  (* precondition : n >= 0 *)
  if n <= 1 then
    n
  else
    fibo (n-1) + fibo (n-2)
(* Write a new version of this function with linear complexity in
   the parameter n. *)
(* Hint. You can use an auxiliary function using two accumulators. *)
let fiboiter n =
  let rec aux a b n =
    match n with 
    | 0 -> a 
    | n -> aux b (a+b) (n-1)
  in aux 0 1 n

let _ = assert (fiboiter 0 = 0)
let _ = assert (fiboiter 1 = 1)
let _ = assert (fiboiter 2 = 1)
let _ = assert (fiboiter 3 = 2)
let _ = assert (fiboiter 4 = 3)
let _ = assert (fiboiter 5 = 5)
let _ = assert (fiboiter 6 = 8)

(* ------------------------------------------------------------------------------ *)
  
(* Exrcise 3: Strings *)
(* (a) palindrome such that palindrome m returns true if and only if the string
       m is a palindrome
       (that is, we see the same sequence of characters whether we read it from
       left to right or from right to left, e.g. madam) *)
let reverse s =
  let l = String.length s
  in String.init l (fun i -> s.[l-i-1])

let palindrome m = m = reverse m

let _ = assert (false = palindrome "cat")
let _ = assert (true = palindrome "racecar")

(* (b) compare such that compare m1 m2 returns true if and only if the string
       m1 is smaller in lexicographic order than the string m2
       (that is, m1 would appear before m2 in a dictionary); *)
let compare s1 s2 = s1 < s2

(* (c) factor such that factor m1 m2 returns true if and only if the string m1
       is a factor of the string m2
       (that is, m1 appears as is in m2). *)
let take n str = String.sub str 0 n
let tail s = String.sub s 1 ((String.length s) - 1)

let rec factor s1 s2 =
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  if l1 < l2 then false
  else if s1 = take l1 s2 then true
  else factor s1 (tail s2)
  
(* ------------------------------------------------------------------------------ *)

(* Exercise 4: Merge sort *)
let rec split l =
  let cat (x, y) (xs, ys) = x :: xs , y :: ys in
  match l with
  | [] -> ([], [])
  | x :: [] -> ([x],[])
  | x :: x2 :: xs -> cat (x,x2) (split xs)

let rec merge l1 l2 =
  match (l1,l2) with
  | [], rest | rest, [] -> rest
  | x :: xs, y :: ys ->
      if x < y
      then x :: merge xs (y :: ys)
      else y :: merge (x :: xs) ys

let rec msort l =
  if List.length l < 2 then l
  else
    let (ll, lr) = split l
    in merge (msort ll) (msort lr)

let xs = [1;3;2;5;3;9;0;0;4]
let _ = msort xs

(* ------------------------------------------------------------------------------ *)

(* Exercise 5: Lists *)
(* (a) square_sum such that square_sum l returns the sum of
       the square of the integers in the list l. *)
let square_sum = List.fold_left (fun acc a -> acc + a*a) 0 
    
(* (b) find_opt such that find_opt x l returns Some i if the element x appears
 in the index i of the list l (but not before), and None if x does not appear in the list l *) 
let rec find_opt x l = 
  match l with 
  | []      -> None 
  | y :: ys -> 
      if x = y then Some x
      else find_opt x ys

(* ------------------------------------------------------------------------------ *)

(* Exercise 7: Concatenation *)
(* Here is a way to code the concatenation of two lists in OCaml. *)
let rec concat l1 l2 = match l1 with
  | [] -> l2
  | x::s -> x :: (concat s l2)
(* This function, like the @ operator provided by OCaml, has a cost proportional to the length
   of the first list. In order to be able to perform multiple concatenations without fear of their
   cost, we propose a new representation of sequences, based on the following data type (which
   can be seen as a concatenation tree). *)
type 'a seq =
  | Elt of 'a
  | Seq of 'a seq * 'a seq
(* The concatenation of two sequences s1 and s2 is therefore simply Seq(s1, s2). We can give
   ourselves a writing shortcut s1 @@ s2 with the definition *)
let (@@) x y = Seq(x, y)
(* Such a tree represents a sequence, obtained by considering all its elements in order from left
   to right. Both trees Seq(Elt 1, Seq(Elt 2, Elt 3)) and Seq(Seq(Elt 1, Elt 2), Elt 3)
   are the two possible representations of the list [1; 2; 3]. *)

(* (a) hd, tl, mem, rev, map, fold_left, fold_right corresponding to functions of the
       same name on lists; *)
let rec hd = function
  | Elt x      -> x
  | Seq (x, y) -> hd x
let rec tl = function
  | Elt x            -> Elt x
  | Seq ((Elt x), y) -> y
  | Seq (x,       y) -> Seq(tl x, y)
let rec mem a = function
  | Elt x     -> a = x
  | Seq (x,y) -> mem a x || mem a y
let rec rev = function
  | Elt x     -> Elt x
  | Seq (x,y) -> Seq (rev y, rev x)
let rec map f = function
  | Elt x     -> Elt (f x)
  | Seq (x,y) -> Seq (map f x,map f y)
let rec fold_left f init = function
  | Elt x     -> f init x
  | Seq (x,y) -> init (* TODO *)
let rec fold_right f l init =
  match l with
  | Elt x     -> f x init
  | Seq (x,y) -> init (* TODO *) 
                      
(* (b) seq2list such that seq2list s returns a OCaml list representing the sequences (do not
   use @);
   Bonus (difficult): give a tail recursive version of this function; *)
let seq2list s = fold_right (fun a acc -> a :: acc) s []
    
(* (c) find_opt such that find_opt x l returns Some i if the element x appears at index i in
       the sequence represented by i (but not before) and None if x does not appear in s; *)
let rec find_opt' a = function
  | Elt x    -> if x = a then Some x else None
  | Seq(x,y) ->
      match (find_opt' a x, find_opt' a y) with
      | Some x, _
      | _, Some x -> Some x
      | _         -> None
        
(* (d) *)
let nth s n = () (* TODO *)
