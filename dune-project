(lang dune 3.6)

(name dependent_lambda)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name dependent_lambda)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune opal camlp-streams)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
