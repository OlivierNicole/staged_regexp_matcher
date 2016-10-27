open ^Pervasives

static of_char c =
  << Pervasives.char_of_int $(Expr.of_int (int_of_char c)) >>

type str = char list

(* A continuation is a dynamic (phase-0) matcher on a string. *)
type cont = str expr -> bool expr

(* Abstract representation of a continuation with possibly static info. *)
type cont_sd =
  | One (* matches the empty string *)
  | Lit of char (* matches a single character *)
  | Dyn of cont (* opaque *)

(* Matches the empty string. *)
static null : cont =
  fun (cs : str expr) ->
    let open Pervasives in
    << match $cs with [] -> true | _ -> false >>

static dynamize_cont (k : str expr -> bool expr) : (str -> bool) expr =
  << fun cs -> $(k <<cs>>) >>

static statize_cont (k : (str -> bool) expr) : str expr -> bool expr =
  fun cs -> << $k $cs >>

static rec unsd_cont : cont_sd -> cont =
  function
  | One -> null
  | Lit c ->
      fun cs ->
        let open Pervasives in
        <<
          match $cs with
          | [c'] -> c' = $(of_char c)
          | _ -> false
        >>
    (*
  | Plus (k, l) ->
      fun cs ->
        let open Pervasives in
        <<
          $(unsd_cont k cs) || $(unsd_cont l cs)
        >>
        *)
  | Dyn k -> k

type regexp = cont_sd -> cont_sd

static zero _ =
  Dyn (fun _ -> <<false>>)
static one k = k
static lit c k =
  match k with
  | One ->
      Lit c
  | _ ->
    Dyn (fun cs ->
      let open Pervasives in
      <<
        match $cs with
        | [] -> false
        | c' :: cs' -> $(of_char c) = c' && $(unsd_cont k <<cs'>>)
      >>)

static plus (r : regexp) (s : regexp) : regexp =
  fun k ->
    match (r k, s k) with
    | (Lit c, Lit c') when c = c' ->
        Lit c
    | _ ->
      Dyn (fun cs ->
        let open Pervasives in
        <<
          $(unsd_cont (r k) cs) || $(unsd_cont (s k) cs)
        >>)

static cat (r : regexp) (s : regexp) : regexp =
  fun k ->
    Dyn (fun cs ->
      let open Pervasives in
      <<
        $(unsd_cont
          (r (Dyn (fun cs' -> << $(unsd_cont (s k) cs') >>)))
          cs)
      >>)

    (*
static star r k cs =
  let open Pervasives in
  <<
    let rec star_dyn (k : str -> bool) (cs : str) : bool =
      k cs || $(r (statize_cont <<fun cs' -> star_dyn k cs'>>) <<cs>>)
    in
    star_dyn $(dynamize_cont k) $cs
  >>
  *)

static ( *.* ) r s = cat r s
static ( +.+ ) r s = plus r s

static match_ (r : regexp) (cs : str expr) : bool expr =
  unsd_cont (r One) cs

static compile (r : regexp) : (str -> bool) expr =
  << fun cs -> $(match_ r <<cs>>) >>

static r = let r' = lit 'a' in r' +.+ r'
