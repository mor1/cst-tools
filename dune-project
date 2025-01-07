(lang dune 3.17)

(name breakdown)

(generate_opam_files true)

(source
 (github mor1/cst-tools))

(authors "Richard Mortier <mort@cantab.net>")

(maintainers "Richard Mortier <mort@cantab.net>")

(license MIT)

(package
 (name breakdown)
 (synopsis "Process CST mark spreadsheets")
 (description "Generate some nice ASCII summaries of individual's marks")
 (depends ocaml dune notty csv containers)
 (tags
  ()))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
