open ^Pervasives
(*
type 'a ps =
  | Sta of 'a
  | Dyn of 'a expr

type 'a list_ps =
  | Nil
  | (^::) of 'a * list_ps
  | DynL of 'a list expr
  *)

type uchar = Nfa.uchar
type character = Nfa.character
type str = Nfa.str

type nfa =
  | StaA of state ^Lazy.t
  | DynA of Nfa.nfa expr
and state = State of bool * transition list
and transition = uchar * nfa

static rec unps_nfa = function
  | StaA s ->
      DynA <<

(*
static nfa m ts = ^Lazy.from_val (State (m, ts))
*)

static rec matches (a : nfa) (cs : str expr) : bool expr =
  match a with
  | DynA a ->
      let open Pervasives in
      <<
        Nfa.matches $a $cs
      >>
  | StaA s ->
      let (m, ts) = ^Lazy.force s in
      let open Pervasives in
      <<
        match $cs with
        | [] -> $(if m then <<true>> else
            << false >>)
      >>

static compile (a : nfa) : (str -> bool) expr =
  << fun cs -> $(matches a <<cs>>) >>
