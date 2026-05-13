(lang dune 3.0)
(generate_opam_files true)

(name kat)
(version 0.1.0)

(package
 (name kat)
 (synopsis "Kleene Algebra with Tests — Brzozowski derivatives & DFA construction")
 (depends
  (ocaml (>= 5.0))
  (dune (>= 3.0))
  hashcons
  ppx_sexp_conv
  ppx_custom_printf
  ppx_enumerate
  (js_of_ocaml (>= 5.0))
  (js_of_ocaml-ppx (>= 5.0))))
