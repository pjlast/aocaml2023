(lang dune 3.11)

(name aocaml2023)

(generate_opam_files true)

(source
 (github pjlast/aocaml2023))

(authors "Petri-Johan Last")

(maintainers "Petri-Johan Last")

(license MIT)

(documentation https://github.com/pjlast/aocaml2023)

(package
 (name aocaml2023)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune base ppx_inline_test)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
