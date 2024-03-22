NUX QR Format
=============

This library supports the QR format used by NUX Mighty Plug guitar amplifiers.

Installation
------------

```sh
$ opam switch create ./ ocaml-base-compiler.4.14.2
$ opam install . --deps-only --with-test
$ dune build
```

CLI
---

Aside from the library interface there exists a binary that can be used from
the shell that allows to work with the data contained in the QR code.

```sh
$ dune exec nux-qr-format
```
