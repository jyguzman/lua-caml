(lang dune 3.17)

(name luacaml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Jordie Guzman <jordieguzman@gmail.com>")

(maintainers "Jordie Guzman <jordieguzman@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name luacaml)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
