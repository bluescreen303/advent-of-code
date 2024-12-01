#!/usr/bin/env -S jaq -Rsf

def parse:
  split("\n")
  | map( select(. != "")
       | split("\\s+"; "")
       | map(tonumber)
       )
;

parse
| transpose
| map(sort)
| transpose
| map(.[0] - .[1] | abs)
| add
