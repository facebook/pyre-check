(lang dune 3.4)

(name errpy)

(version 0.0.10)

(generate_opam_files true)

(maintainers "pyre@meta.com")

(authors "pyre@meta.com")

(bug_reports "https://github.com/facebook/errpy/issues")

(homepage "https://github.com/facebook/errpy")

(source (github "facebook/errpy" ) )

(formatting
  (enabled_for dune)
)

(license MIT)

(package
 (name errpy)
 (synopsis "Errpy: An Error Recovering Python Parser implemented in Rust")
 (description "This is a new project to provide an error-recovering Python parser, implemented in Rust based on tree-sitter. Our short-term goal is to use it in the pyre type checker, in order to provide better IDE features.")
 (depends ocaml dune ppx_deriving (conf-rust-2021 ( and :build)) (ounit2 (and :with-test)))
 )
