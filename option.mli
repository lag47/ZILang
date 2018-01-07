val return : 'a -> 'a option
val bind : 'a option -> ('a -> 'b option) -> 'b option
val map : 'a option -> ('a -> 'b) -> 'b option
val join : 'a option option -> 'a option
val extract: 'a -> 'a option -> 'a
val (>>=) : 'a option -> ('a -> 'b option) -> 'b option
val (>>|) : 'a option -> ('a -> 'b) -> 'b option
