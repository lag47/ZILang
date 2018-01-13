type 'a t =
  | Solution of 'a
  | Nosol of string

val solvemod: int -> int -> int -> (int * int) t

val invmod: int -> int -> (int * int) t

val chin_rem: (int * int) list -> (int * int) t
