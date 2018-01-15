# zilang
zilang is an interpretted language implemented in Ocaml with native support solving basic congruences, as well as anonymous functions.
It includes a REPL in which it is easy to define and solve equivalence classes, linear modular equivalences, and systems of linear modular equivalences.
Independent from that main point, the anonymous functions allow for the simulation of untyped lambda calculus. For laughs, I included the encodings of a number and some basic functions in [Church Numerals](https://en.wikipedia.org/wiki/Church_encoding)
## Why?
Mostly to learn how to work with lexers and parse generators . Also, I just finished a number theory class where this might have been somewhat helpful.
## Inspirations
All of my knowledge of implementing interpretted languages come from assignments from either [CS2112](http://www.cs.cornell.edu/courses/cs2112) or [CS3110](http://www.cs.cornell.edu/courses/cs3110)
## Acknowledgements
[menhir](https://github.com/pippijn/menhir)
[ocamllex](https://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html)
