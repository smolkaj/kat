open Js_of_ocaml
open Kat
open Sexplib0.Sexp

let test_of_string s =
  try Some (test_of_sexp (Atom s)) with _ -> None
let action_of_string s =
  try Some (action_of_sexp (Atom s)) with _ -> None

let enumerate_atoms () : atom list =
  let n = List.length all_of_test in
  let count = 1 lsl n in
  List.init count (fun i ->
    let assignment = List.mapi (fun j t -> (t, i land (1 lsl j) <> 0)) all_of_test in
    fun t -> List.assoc t assignment)

let pp_atom (a : atom) : string =
  all_of_test
  |> List.filter_map (fun t ->
    if a t then Some (to_string (sexp_of_test t))
    else None)
  |> function
    | [] -> "∅"
    | ts -> String.concat "" ts

(* ========================================================================= *)
(* PARSER                                                                    *)
(* Simple recursive descent parser for KAT expressions.                      *)
(*   e ::= e '+' e  |  e e  |  e '*'  |  '!' e  |  '(' e ')'               *)
(*       | 'T1' .. 'T4'  |  'A1' .. 'A4'  |  '0'  |  '1'                   *)
(* ========================================================================= *)

type token =
  | TTest of test
  | TAction of action
  | TZero | TOne
  | TPlus | TStar | TBang
  | TLParen | TRParen
  | TSemicolon
  | TEOF

let tokenize (s : string) : token list =
  let i = ref 0 in
  let n = String.length s in
  let tokens = ref [] in
  while !i < n do
    match s.[!i] with
    | ' ' | '\t' | '\n' | '\r' -> incr i
    | '+' -> tokens := TPlus :: !tokens; incr i
    | '*' -> tokens := TStar :: !tokens; incr i
    | '!' | '~' -> tokens := TBang :: !tokens; incr i
    | '(' -> tokens := TLParen :: !tokens; incr i
    | ')' -> tokens := TRParen :: !tokens; incr i
    | ';' -> tokens := TSemicolon :: !tokens; incr i
    | '0' -> tokens := TZero :: !tokens; incr i
    | '1' -> tokens := TOne :: !tokens; incr i
    | 'T' | 'A' when !i + 1 < n ->
      let name = String.sub s !i 2 in
      (match test_of_string name with
       | Some t -> tokens := TTest t :: !tokens; i := !i + 2
       | None ->
         match action_of_string name with
         | Some a -> tokens := TAction a :: !tokens; i := !i + 2
         | None -> failwith (Printf.sprintf "unexpected: %s" name))
    | c -> failwith (Printf.sprintf "unexpected character: %c" c)
  done;
  List.rev !tokens

(* Precedence, low to high: union, seq, star, not, atom *)

let parse (tokens : token list) : Exp.t =
  let toks = ref tokens in
  let peek () = match !toks with t :: _ -> t | [] -> TEOF in
  let advance () = match !toks with _ :: rest -> toks := rest | [] -> () in
  let expect t =
    if peek () = t then advance ()
    else failwith "parse error: unexpected token"
  in
  let rec parse_union () =
    let lhs = parse_seq () in
    let rec loop acc =
      match peek () with
      | TPlus -> advance (); loop (Exp.Union (acc, parse_seq ()))
      | _ -> acc
    in
    loop lhs
  and parse_seq () =
    let lhs = parse_postfix () in
    let rec loop acc =
      match peek () with
      | TSemicolon -> advance (); loop (Exp.Seq (acc, parse_postfix ()))
      | TTest _ | TAction _ | TZero | TOne | TBang | TLParen ->
        loop (Exp.Seq (acc, parse_postfix ()))
      | _ -> acc
    in
    loop lhs
  and parse_postfix () =
    let e = parse_prefix () in
    let rec loop acc =
      match peek () with
      | TStar -> advance (); loop (Exp.Star acc)
      | _ -> acc
    in
    loop e
  and parse_prefix () =
    match peek () with
    | TBang -> advance (); Exp.Test (Exp.Not (parse_bexp_atom ()))
    | _ -> parse_atom ()
  and parse_bexp_atom () =
    match peek () with
    | TTest t -> advance (); Exp.Test t
    | TBang -> advance (); Exp.Not (parse_bexp_atom ())
    | TLParen ->
      advance ();
      let b = parse_bexp_union () in
      expect TRParen;
      b
    | _ -> failwith "parse error: expected boolean expression after '!'"
  and parse_bexp_union () =
    let lhs = parse_bexp_seq () in
    let rec loop acc =
      match peek () with
      | TPlus -> advance (); loop (Exp.Or (acc, parse_bexp_seq ()))
      | _ -> acc
    in
    loop lhs
  and parse_bexp_seq () =
    let lhs = parse_bexp_atom () in
    let rec loop acc =
      match peek () with
      | TTest _ | TBang | TLParen ->
        loop (Exp.And (acc, parse_bexp_atom ()))
      | _ -> acc
    in
    loop lhs
  and parse_atom () =
    match peek () with
    | TTest t -> advance (); Exp.Test (Exp.Test t)
    | TAction a -> advance (); Exp.Action a
    | TZero -> advance (); Exp.abort
    | TOne -> advance (); Exp.skip
    | TLParen ->
      advance ();
      let e = parse_union () in
      expect TRParen;
      e
    | _ -> failwith "parse error: unexpected token"
  in
  let e = parse_union () in
  if peek () <> TEOF then failwith "parse error: unexpected token after expression";
  e

let parse_string (s : string) : Exp.t =
  parse (tokenize s)


module StateMap = Map.Make(struct
  type t = ExpACI.t
  let compare = ExpACI.compare
end)

let js_string s = Js.Unsafe.inject (Js.string s)
let js_int n = Js.Unsafe.inject n
let js_bool b = Js.Unsafe.inject (Js.bool b)

let js_array_of_list xs =
  Js.Unsafe.inject (Js.array (Array.of_list xs))

let explore_dfa (e : Exp.t) =
  let dfa = ExpACI.brzozowski_dfa e in
  let atoms = enumerate_atoms () in
  let queue = Queue.create () in
  let state_id = ref StateMap.empty in
  let next_id = ref 0 in
  let get_id s =
    match StateMap.find_opt s !state_id with
    | Some id -> (id, false)
    | None ->
      let id = !next_id in
      incr next_id;
      state_id := StateMap.add s id !state_id;
      (id, true)
  in
  let states = ref [] in
  let transitions = ref [] in
  Queue.push dfa.start queue;
  ignore (get_id dfa.start);
  while not (Queue.is_empty queue) do
    let s = Queue.pop queue in
    let (id, is_new) = get_id s in
    if is_new || id = 0 && List.length !states = 0 then begin
      let label = Format.asprintf "%a" ExpACI.pp s in
      let accepting =
        atoms
        |> List.filter (fun a -> dfa.obs s a)
        |> List.map (fun a -> js_string (pp_atom a))
      in
      states := Js.Unsafe.obj [|
        ("id", js_int id);
        ("label", js_string label);
        ("start", js_bool (id = 0));
        ("accepting", js_array_of_list accepting);
      |] :: !states;
      List.iter (fun a ->
        List.iter (fun p ->
          let s' = dfa.trans s a p in
          if not (ExpACI.is_abort s') then begin
            let (id', is_new') = get_id s' in
            let edge_label = Printf.sprintf "%s,%s" (pp_atom a) (to_string (sexp_of_action p)) in
            transitions := Js.Unsafe.obj [|
              ("from", js_int id);
              ("to", js_int id');
              ("label", js_string edge_label);
            |] :: !transitions;
            if is_new' then Queue.push s' queue
          end) all_of_action) atoms
    end
  done;
  (dfa.start, js_array_of_list (List.rev !states), js_array_of_list (List.rev !transitions))

let () =
  let process input_str =
    try
      let e = parse_string input_str in
      let (start, states, transitions) = explore_dfa e in
      let pretty = Format.asprintf "%a" ExpACI.pp start in
      Js.Unsafe.obj [|
        ("ok", Js.Unsafe.inject Js._true);
        ("expression", Js.Unsafe.inject (Js.string pretty));
        ("states", states);
        ("transitions", transitions);
      |]
    with Failure msg ->
      Js.Unsafe.obj [|
        ("ok", Js.Unsafe.inject Js._false);
        ("error", Js.Unsafe.inject (Js.string msg));
      |]
  in
  Js.export "KAT" (Js.Unsafe.obj [|
    ("process", Js.Unsafe.inject (Js.wrap_callback (fun s ->
      process (Js.to_string s))));
  |])
