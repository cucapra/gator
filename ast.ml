
(* AST definition of IMP Language *)
(* https://www.cs.cornell.edu/courses/cs6110/2018sp/lectures/lec08.pdf *)

type aexp = 
	Num of int
	| Var of string
	| Plus of aexp * aexp
	| Times of aexp * aexp
	| Minus of aexp * aexp

type bexp = 
	True
	| False
	| Eq of aexp * aexp
	| Leq of aexp * aexp
	| Or of bexp * bexp
	| And of bexp * bexp
	| Not of bexp

type comm = 
	Skip
	| Assign of string * aexp
	| Comp of comm * comm
	| If of bexp * comm * comm
