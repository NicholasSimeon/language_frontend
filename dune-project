(lang dune 3.15)

(name language_frontend)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Nicholas Simeon")

(maintainers "Nicholas Simeon")

(license MIT)

(documentation https://github.com/NicholasSimeon/LanguageFrontend)

(package
 (name language_frontend)
 (synopsis "Extensible ocaml programming language lexer + parsing interface")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
