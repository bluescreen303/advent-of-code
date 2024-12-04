#!/usr/bin/env -S jaq -Rsf

def parse:
  split("\n")
  | map( select(. != "")
       | split("")
       )
;

def countXMAS:
  indices(["X","M","A","S"])
  | length
;

def diagonal($b):
    .[0] as $e | .[1:] as $es
  | ($b | map(.[1:]))  as $ts
  | [$b | map(.[0])]
    + if $e == null
      then $ts | transpose | map(map(select(. != null)))
      else $es | diagonal([$e] + $ts)
      end
;

def diagonal: # '/'
  diagonal([])[1:]
;

def diag2:    # '\'
  map(reverse) | diagonal | map(reverse)
;

parse
| transpose       as $t
| diagonal        as $d
| map(reverse)    as $r
| ($r | diagonal) as $rd
| [   .,   $r
  ,  $t,  ($t | map(reverse))
  ,  $d,  ($d | map(reverse))
  , $rd, ($rd | map(reverse))
  ]
| map(map(countXMAS)[])
| add
