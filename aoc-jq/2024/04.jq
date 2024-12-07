#!/usr/bin/env -S jaq -L . -Rsf

import "aoc-jq/utils" as utils;

def parse:
  split("\n")[]
  | select(. != "")
  | split("")
;

def countXMAS:
  indices(["X","M","A","S"]) | length
;

def puzzle_a:
    transpose              as $t
  | utils::diagonal        as $d
  | map(reverse)           as $r
  | ($r | utils::diagonal) as $rd
  | [   .,   $r
    ,  $t,  ($t | map(reverse))
    ,  $d,  ($d | map(reverse))
    , $rd, ($rd | map(reverse))
    ]
  | map(map(countXMAS)[])
  | add
;

def find($needle):
  to_entries[]
  | (.key | tonumber) as $k
  | .value
  | indices($needle)[]
  | [$k, .]
;

def puzzle_b:
  . as $input
  | [ find("A")                                                 # find 'A's
    | select(     .[0] > 0                                      # not at a border/side
              and .[1] > 0
              and .[0] < ($input    | length - 1)
              and .[1] < ($input[0] | length - 1)
            )
    | . as [$y, $x]
    | $input
    | [ ([.[$y-1][$x-1], .[$y][$x], .[$y+1][$x+1]] | join(""))  # select diagonal '\'
      , ([.[$y+1][$x-1], .[$y][$x], .[$y-1][$x+1]] | join(""))  # select diagonal '/'
      ]
    | all(. == "MAS" or . == "SAM")                             # check MAS
    | select(.)                                                 # keep successes only
    ]
  | length                                                      # and count them
;

[parse]
| puzzle_a, puzzle_b
