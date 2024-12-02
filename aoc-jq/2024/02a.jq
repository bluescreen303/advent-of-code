#!/usr/bin/env -S jaq -Rsf

def parse:
  split("\n")
  | map( select(. != "")   # drop empty line
       | split("\\s+"; "") # split by 1 or more whitespace
       | map(tonumber)
       )
;

def safe:
  [.[:-1], .[1:]]             # put input (minus last item) next to input (minus first item)
  | transpose                 # to get neighbouring pairs
  | map(.[1] - .[0])          # get difference for each pair
  |  all(. >=  1 and . <=  3)
  or all(. <= -1 and . >= -3)
;

parse
| map(select(safe))
| length
