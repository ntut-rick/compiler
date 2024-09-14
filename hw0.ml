(* Exercise 1: Number warm-up *)
(* (a) fact such that fact n returns the factorial of the positive integer n. *)
let rec fact n =
  if n = 0 then 1
  else n * fact (n-1)

let _ = assert (fact 4 = 24)
let _ = assert (fact 5 = 120)

(* (b) nb_bit_pos such that bn_bit_pos n returns the number of bits equal to 1
       in the binary representation of the positive integer n *)
let rec nb_bit_pos n =
  if n = 0 then 0
  else n land 1 + nb_bit_pos (n lsr 1)

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
    if n = 0 then a
    else aux b (a+b) (n-1)
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
