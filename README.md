<!---
This file was generated from `meta.yml`, please do not edit manually.
Follow the instructions on https://github.com/coq-community/templates to regenerate.
--->
# DNAml

[![Docker CI][docker-action-shield]][docker-action-link]
[![coqdoc][coqdoc-shield]][coqdoc-link]

[docker-action-shield]: https://github.com/CharlesAverill/dnaml/actions/workflows/docker-action.yml/badge.svg?branch=master
[docker-action-link]: https://github.com/CharlesAverill/dnaml/actions/workflows/docker-action.yml


[coqdoc-shield]: https://img.shields.io/badge/docs-coqdoc-blue.svg
[coqdoc-link]: https://CharlesAverill.github.io/dnaml/docs/latest/coqdoc/toc.html


An experimental formalization of ML-style genetic programming in Coq

## Meta

- Author(s):
  - Charles Averill [<img src="https://zenodo.org/static/images/orcid.svg" height="14px" alt="ORCID logo" />](https://orcid.org/0000-0001-6614-1808) (initial)
- License: [MIT License](LICENSE)
- Compatible Coq versions: 8.19.1 or later
- Additional dependencies:
  - [Dune](https://dune.build) 2.5 or later
- Coq namespace: `dnaml`
- Related publication(s): none

## Building and installation instructions

The easiest way to install the latest released version of DNAml
is via [OPAM](https://opam.ocaml.org/doc/Install.html):

```shell
opam repo add coq-released https://coq.inria.fr/opam/released
opam install coq-dnaml
```

To instead build and install manually, do:

``` shell
git clone https://github.com/CharlesAverill/dnaml.git
cd dnaml
dune build
dune install
```


# Design

I'm taking a lot of cues from [PushGP](http://faculty.hampshire.edu/lspector/push.html)

Can currently write a list of genes (a series of AST tokens),
write epigenetic marker functions and "render" a genome
(a list of genes combined with a list of markers) given
the appropriate set of mutations (parametrized over genes and markers)
