type character = char
type str = character list

type uchar =
  | Epsilon
  | Char of character

type nfa = state Lazy.t
and state = State of bool * transition list
  (* state (matching or not) with transitions *)
and transition = uchar * nfa

let nfa m ts = Lazy.from_val (State (m, ts))
let head a = Lazy.force a

(* non-deterministic transition *)
let rec advance (a : nfa) (c : uchar) : nfa list =
  let rec f = function
  | [] -> []
  | (c', a') :: ts ->
      if c = c' then
        a' :: f ts
      else
        f ts
  in
  let State (_, ts) = head a in
  f ts

(* epsilon-transition to [a]. *)
let epsilon (a : nfa) : transition =
  (Epsilon, a)

let zero : nfa =
  nfa false []

let one : nfa =
  nfa true []

let char (c : character) : nfa =
  nfa false [(Char c, nfa true [])]

let cat (a : nfa) (b : nfa) : nfa =
  (* Add an epsilon-transition from all matching states of [a] to [b]. *)
  let rec connect a =
    match head a with
    | State (m, ts) ->
        nfa false (
          let ts' =
            List.map (fun (c, a) -> (c, connect a)) ts
          in
          if m then epsilon b :: ts'
          else ts'
        )
  in
  connect a

let plus (a : nfa) (b : nfa) : nfa =
  nfa false [epsilon a; epsilon b]

let rec star (a : nfa) : nfa =
  lazy (State (true,
    [epsilon (cat a (star a))
    ]))

let ( *.* ) a b = cat a b
let ( +.+ ) a b = plus a b

let maybe (a : nfa) = a +.+ one

let matching a =
  let State (m, _) = head a in m

let rec any_true f =
  function
  | [] -> false
  | x :: xs ->
      if f x then true
      else any_true f xs

let rec matches (a : nfa) (cs : str) =
  match cs with
  | [] -> matching a || any_true matching (advance a Epsilon)
  | c :: cs' ->
      any_true (fun a -> matches a cs) (advance a Epsilon) ||
        any_true (fun a -> matches a cs') (advance a (Char c))

let rec repeat n x =
  match n with
  | 0 -> []
  | n -> x :: repeat (pred n) x
