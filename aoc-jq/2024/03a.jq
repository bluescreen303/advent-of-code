#!/usr/bin/env -S jaq -Rsf

def one_mul:
  .captures
  | map(.string | tonumber)
  | .[0] * .[1]
;

[ match("mul\\(([0-9]+),([0-9]+)\\)"; "gm")
  | one_mul
] | add
