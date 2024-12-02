#!/usr/bin/env -S jaq -Rsf

def parse:
  split("\n")
  | map( select(. != "")   # drop empty line
       | split("\\s+"; "") # split by 1 or more whitespace
       | map(tonumber)
       )
;

parse
| transpose
| map(sort)
| transpose
| map(.[0] - .[1] | abs)
| add
