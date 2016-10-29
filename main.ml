open ^Pervasives

(* must be equalizable and comparable *)
type character = char
type str = string

static expr_of_char c =
  << Pervasives.char_of_int $(Expr.of_int (int_of_char c)) >>

(* A continuation is a dynamic (phase-0) matcher on a string. *)
type cont = str expr -> bool expr

(* Abstract representation of a continuation with possibly static info. *)
type cont_sd =
  | One (* matches the empty string *)
  | Lit of string (* matches a known string *)
  | Any (* matches any single character *)
  | Dyn of cont (* opaque *)

(* Matches the empty string. *)
static null : cont =
  fun (cs : str expr) ->
    let open Pervasives in
    << $cs = "" >>

static dynamize_cont (k : str expr -> bool expr) : (str -> bool) expr =
  << fun cs -> $(k <<cs>>) >>

static statize_cont (k : (str -> bool) expr) : str expr -> bool expr =
  fun cs -> << $k $cs >>

static rec unsd_cont : cont_sd -> cont =
  function
  | One -> null
  | Lit s ->
      fun cs ->
        let open Pervasives in
        <<
          $cs = $(Expr.of_string s)
        >>
  | Any ->
      fun cs ->
        let open Pervasives in
        <<
          String.length $cs = 1
        >>
  | Dyn k -> k

type regexp = cont_sd -> cont_sd

static zero _ =
  Dyn (fun _ -> <<false>>)
static one k = k
static lit s k =
  match k with
  | One ->
      Lit s
  | Lit s' ->
      Lit (s ^ s')
  | _ ->
    Dyn (fun cs ->
      let open Pervasives in
      <<
        let s' = $cs in
        let l = $(Expr.of_int (^String.length s)) in
        let l' = String.length s' in
        l' >= l &&
          String.sub s' 0 l = $(Expr.of_string s) &&
          $(unsd_cont k <<String.sub s' l (l' - l)>>)
      >>)

static plus (r : regexp) (s : regexp) : regexp =
  fun k ->
    match (r k, s k) with
    | (Lit c, Lit c') when c = c' ->
        Lit c
    | (Any, Any) ->
        Any
    | _ ->
      Dyn (fun cs ->
        let open Pervasives in
        <<
          $(unsd_cont (r k) cs) || $(unsd_cont (s k) cs)
        >>)

static cat (r : regexp) (s : regexp) : regexp =
  fun k ->
    match s k with
    | One -> r k
    | k' -> r k'

static star (r : regexp) : regexp =
  fun k ->
    match r k with
    | One -> One
    | _ ->
      Dyn (fun cs ->
        let open Pervasives in
        <<
          let rec star_dyn (k : str -> bool) (cs : str) : bool =
            k cs
            || $(unsd_cont (r (Dyn (statize_cont <<fun cs' -> star_dyn k cs'>>))) <<cs>>)
          in
          star_dyn $(dynamize_cont (unsd_cont k)) $cs
        >>)

static any : regexp =
  fun k ->
    Dyn (fun cs ->
      let open Pervasives in
      <<
        let s = $cs in
        String.length s >= 1 &&
          $(unsd_cont k <<String.sub s 1 (pred (String.length s))>>)
      >>)

static ( *.* ) r s = cat r s
static ( +.+ ) r s = plus r s

static maybe r = one +.+ r
static several r = r *.* star r

static range (x : character) (y : character) : regexp =
  fun k ->
    Dyn (fun cs ->
      let open Pervasives in
      <<
        let s = $cs in
        let l = String.length s in
        l >= 1 &&
          (let c = s.[0] in $(expr_of_char x) <= c && c <= $(expr_of_char y)) &&
          $(unsd_cont k <<String.sub s 1 (pred l)>>)
      >>)

(* matches one character if it's different from x. *)
static not_ (x : character) : regexp =
  fun k ->
    Dyn (fun cs ->
      let open Pervasives in
      <<
        let s = $cs in
        let l = String.length s in
        l >= 1 && s.[0] <> $(expr_of_char x) &&
          $(unsd_cont k <<String.sub s 1 (pred l)>>)
      >>)

static match_ (r : regexp) (cs : str expr) : bool expr =
  unsd_cont (r One) cs

static compile (r : regexp) : (str -> bool) expr =
  << fun cs -> $(match_ r <<cs>>) >>

  (*
open Pervasives

let () =
  let ic = open_in "test.txt" in
  let str = input_line ic in
  let matcher = $(compile r) in
  for i = 1 to 100000 do
    assert (
      matcher str
    )
  done;
  close_in ic
  *)
