(lang dune 3.17)

(name ohttp)

(generate_opam_files true)

(package
 (name ohttp)
 (depends ocaml dune eio_main angstrom ppx_let (ppx_inline_test :with-test) (ppx_assert :with-test) sexplib0))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
