(**Mathematical functions used to implement functionality*)

(**The type of a result of trying to solve a problem that may not have a
 * solution*)
type 'a t =
  | Solution of 'a
  | Nosol of string

(**[gcd a b] is the greatest common denominator of a and b*)
val gcd: int -> int -> int

(**if [solvemod a m b = Solution(c,n)] then
 * if c' equivalent to c (mod n) then a * c' equivalent to b (mod m)
 * if [solvemod a m b = Nosol s] then there is no k such that
 * a * k equivalent to b (mod m) *)
val solvemod: int -> int -> int -> (int * int) t

(**if [invmod a m equivalent to Solution(c,n)] then
 * if c' equivalent to c (mod n) then a * c' equivalent to 1 (mod m)
 * otherwise a does not have an inverse mod m*)
val invmod: int -> int -> (int * int) t

(**if [chin_rem classes = Solution (a,m)] then the equivalence class of
 * a (mod m) is the solution to the series of equivalences represented by
 * classes
 * otherwise the system has no solution*)
val chin_rem: (int * int) list -> (int * int) t
