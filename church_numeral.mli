(**Represent natural numbers as untyped functions in the Church Numeral
 * encoding*)

open Ast

    (**phrase defining a Church numeral representing 0*)
val zero: phrase

(**phrase defining a Church numeral representing 1*)
val one: phrase

(**phrase defining the addition function on Church numerals*)
val plus: phrase

(**phrase defining the multiplication function on Church numerals*)
val mult: phrase

(**phrase defining the exponentiation function on Church numerals*)
val pow: phrase

(**phrase defining a function which takes Church numerals to an
 * int representing the same integer*)
val decode: phrase
