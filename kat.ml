(*===========================================================================*)
(* PARAMETERS                                                                *)
(*===========================================================================*)

(** Primitive tests. May want to functorize over this type. *)
type test = T1 | T2 | T3 | T4

(** Actions. The set of actions is often denoted by Σ. *)
type action = A1 | A2 | A3 | A4

(** Atoms are truth assignments, mapping tests to true/false. *)
type atom = test -> bool


(*===========================================================================*)
(* AUTOMATA                                                                  *)
(*===========================================================================*)

(** deterministic KAT automaton - possibly with infinitely many states. *)
type 'state dfa = {
  start : 'state;
  obs : 'state -> atom -> bool;
  trans : 'state -> atom -> action -> 'state;
}


(*===========================================================================*)
(* EXPRESSIONS                                                               *)
(*===========================================================================*)

(** KAT expressions, as usually defined on paper. *)
module Exp = struct

  (* boolean expressions *)
  type b =
    | True
    | False
    | Test of test
    | And of b * b
    | Or of b * b
    | Not of b

  (* KAT expressions *)
  type t =
    | Test of b
    | Action of action
    | Seq of t * t
    | Union of t * t
    | Star of t

  let skip = Test True
  let abort = Test False


  (*=========================================================================*)
  (* BRZOZOWKSI DERIVATIVES & DFA                                            *)
  (* See, for example, Section 15.4.2 in the following paper:                *)
  (*  http://www.cs.cornell.edu/~kozen/Papers/ChenPucella.pdf                *)
  (*=========================================================================*)

  let rec eval_bexp (b : b) (a : atom) : bool =
    match b with
    | True -> true
    | False -> false
    | Test t -> a t
    | And (c1, c2) -> eval_bexp c1 a && eval_bexp c2 a
    | Or (c1, c2) -> eval_bexp c1 a || eval_bexp c2 a
    | Not c -> not (eval_bexp c a)

  let rec epsilon (e : t) (a : atom) : bool =
    match e with
    | Test b -> eval_bexp b a
    | Action _ -> false
    | Seq (f1, f2) -> epsilon f1 a && epsilon f2 a
    | Union (f1, f2) -> epsilon f1 a || epsilon f2 a
    | Star _ -> true

  let rec delta (e : t) (a : atom) (p : action) : t =
    match e with
    | Test _ ->
      abort
    | Action q ->
      if p = q then skip else abort
    | Seq (f1, f2) ->
      let d = Seq (delta f1 a p, f2) in
      if epsilon f1 a then
        Union (d, delta f2 a p)
      else
        d
    | Union (f1, f2) ->
      Union (delta f1 a p, delta f2 a p)
    | Star f ->
      Seq (delta f a p, Star f)

  (** The Brzozowksi automaton for a given expression.
      Caveat: The state space of this automaton is the set of expressions,
      which is infinite. 
  *)
  let brzozowski_dfa (e : t) : t dfa =
    { start = e;
      obs = epsilon;
      trans = delta;
    }
end


(*===========================================================================*)
(* EXPRESSIONS MODULO ACI                                                    *)
(*===========================================================================*)

(*
The key issue with the Brozozowksi automaton, as defined above, is that it
has infinitely many states. This problem can be solved by equating expressions
that are equivalent modulo ACI (associativity, commutativity, and idempotence).

To make this practical, we define another type of expressions, which is
modulo ACI by construction.
*)


(** KAT expressions modulo ACI. *)
module ExpACI = struct

  open Hashcons
  (** Consult the following links for documentation:
      * https://en.wikipedia.org/wiki/Hash_consing
      * https://www.lri.fr/~filliatr/ftp/publis/hash-consing2.pdf
      * https://github.com/backtracking/ocaml-hashcons/blob/master/hashcons.mli
  *)

  type t = node Hashcons.hash_consed
  and node =
    | Test of test * bool             (** positive or negated test *)
    | Action of action
    | Seq of t list                   (** modulo associativity *)
    | Union of node Hashcons.Hset.t   (** modulo associativity, commutativity, idempotence *)
    | Star of t

  let tbl = Hashcons.create 10000
  let hashcons : node -> t = Hashcons.hashcons tbl

  (** skip and abort, also know as true and false *)
  let skip = hashcons (Seq [])
  let abort = hashcons (Union Hashcons.Hset.empty)

  let equal e f = (e.tag = f.tag)
  let compare e f = Pervasives.compare e.tag f.tag
  let hash e = e.hkey

  let is_skip e = equal e skip
  let is_abort e = equal e abort

  let mk_seq (e: t) (f: t) : t =
    let sequents e =
      match e.node with
      | Seq xs -> xs
      | _ -> [e]
    in
    if is_abort e || is_abort f then abort else
    match sequents e @ sequents f with
    | [x] -> x
    | xs -> hashcons (Seq xs)

  let mk_union (e: t) (f: t) : t =
    let open Hashcons.Hset in
    let disjuncts e =
      match e.node with
      | Union xs -> xs
      | _ -> singleton e
    in
    let xs = union (disjuncts e) (disjuncts f) in
    match elements xs with
    | [x] -> x
    | _ -> hashcons (Union xs)

  let mk_star (e : t) : t =
    if is_skip e || is_abort e then skip else
    match e.node with
    | Star f -> f
    | _ -> hashcons (Star e)

  let rec of_bexp (b : Exp.b) ~negate : t =
    match b with
    | True -> if negate then abort else skip
    | False -> if negate then skip else abort
    | Test t -> hashcons (Test (t, not negate))
    | Not b -> of_bexp b ~negate:(not negate)
    | And (b1, b2) -> mk_seq (of_bexp b1 ~negate) (of_bexp b2 ~negate)
    | Or (b1, b2) -> mk_union (of_bexp b1 ~negate) (of_bexp b2 ~negate)

  let rec of_exp (e : Exp.t) : t =
    match e with
    | Test b -> of_bexp b ~negate:false
    | Action a -> hashcons (Action a)
    | Seq (e1, e2) -> mk_seq (of_exp e1) (of_exp e2)
    | Union (e1, e2) -> mk_union (of_exp e1) (of_exp e2)
    | Star e -> mk_star (of_exp e)


  (*=========================================================================*)
  (* BRZOZOWKSI DERIVATIVES & DFA                                            *)
  (*=========================================================================*)

  let rec epsilon (e : t) (a : atom) : bool =
    match e.node with
    | Test (t, positive) ->
      if positive then a t else not (a t)
    | Action _ ->
      false
    | Seq es ->
      List.for_all (fun e -> epsilon e a) es
    | Union es -> 
      Hashcons.Hset.exists (fun e -> epsilon e a) es
    | Star _ ->
      true

  let rec delta (e : t) (a : atom) (p : action) : t =
    match e.node with
    | Test _ ->
      abort
    | Action q ->
      if p = q then skip else abort
    | Seq es ->
      delta_seq es abort a p
    | Union es ->
      Hashcons.Hset.elements es
      |> List.map (fun e -> delta e a p)
      |> List.fold_left mk_union abort
    | Star e0 ->
      mk_seq (delta e0 a p) e
  and delta_seq (es : t list) (acc : t) (a : atom) (p : action) : t =
    match es with
    | [] -> acc
    | e::es ->
      let d = mk_seq (delta e a p) (hashcons (Seq es)) in
      let acc = mk_union acc d in
      if epsilon e a then
        delta_seq es acc a p
      else
        acc


  (** The Brzozowksi automaton for a given expression.
      In contrast to Exp.brzozowski_dfa, this function gives a finite state
      (and in fact fairly small) DFA.
  *)
  let brzozowski_dfa (e : Exp.t) : t dfa =
    { start = (of_exp e);
      obs = epsilon;
      trans = delta;
    }

end
