(lang dune 3.14)
(name nux-qr-format)
(generate_opam_files true)

(source
 (github Leonidas-from-XIV/nux-qr-format))
(authors "Marek Kubica <marek@xivilization.net>")
(maintainers "Marek Kubica <marek@xivilization.net>")
(license MIT)

(package
 (name nux-qr-format)
 (synopsis "Library to interact with the format of NUX Mighty amps")
 (description "The NUX Mighty guitar amps support customization through the ")
 (depends
   (ocaml (>= 4.08))
   (stdio (>= v0.15))
   (base (>= v0.15))
   (base64 (>= 3.5))))
