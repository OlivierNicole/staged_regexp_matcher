type character = char
type str = character list
type uchar = Epsilon | Char of character
type nfa = state Lazy.t
and state = State of bool * transition list
and transition = uchar * nfa
val nfa : bool -> transition list -> state Lazy.t
val head : 'a Lazy.t -> 'a
val advance : nfa -> uchar -> nfa list
val epsilon : nfa -> transition
val zero : nfa
val one : nfa
val char : character -> nfa
val cat : nfa -> nfa -> nfa
val plus : nfa -> nfa -> nfa
val star : nfa -> nfa
val ( *.* ) : nfa -> nfa -> nfa
val ( +.+ ) : nfa -> nfa -> nfa
val maybe : nfa -> nfa
val matching : state Lazy.t -> bool
val any_true : ('a -> bool) -> 'a list -> bool
val matches : nfa -> str -> bool
val repeat : int -> 'a -> 'a list
