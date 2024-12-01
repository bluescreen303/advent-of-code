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
| .[1] as $right
| .[0]
| map( . as $me
     | $right | map(select(. == $me))
              | length
              * $me
     )
| add
