# ocaml-dataframe
Simple and (reasonably) type-safe dataframe api implemented in pure ocaml

[![Build Status](https://travis-ci.org/LaurentMazare/ocaml-dataframe.svg?branch=master)](https://travis-ci.org/LaurentMazare/ocaml-dataframe)

This is mostly a work in progress to experiment with how to use the ocaml
type system to provide more type safety to dataframes.
It aims at providing the following functionalites:
- Cover some basic types for columns, int, float, string, ....
- Provide the possibility for the user to add new element types (e.g. for
  date, time), as well as new storage types (e.g. packed int options,
  ...).
- Functional api with immutable dataframes.
- Use applicative for filtering dataframes and for map like operations.
- Read/write from csv with automated type detection.
- Maybe support join ? (TODO)
