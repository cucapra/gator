#!/bin/sh
ocamlc -g  util.cmo str.cma state.cmo print.cmo lin_ops.cmo lexer.cmo ops.cmo check.cmo parser.cmo check_driver.ml
ocamldebug a.out $1
