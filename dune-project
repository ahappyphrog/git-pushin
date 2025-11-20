(lang dune 3.20)

(name git-pushin)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name git-pushin)
 (synopsis "A simple CLI-based calendar application")
 (description "A calendar application for scheduling meetings between attendees and a host")
 (depends ocaml yojson)
 (tags
  ("calendar" "cli" "scheduling")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
