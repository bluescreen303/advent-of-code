#!/usr/bin/env -S jaq -Rsf

def parse:
  split("\n")
  | map( select(. != "")
       | split("")
       )
;

def find($needle):
  to_entries
  | map( (.key | tonumber) as $k
       | .value
       | indices($needle)
       | map([$k, .])[]
       )[]
;

parse
| . as $input
| [ find("A")                                                     # find 'A's
  | select(   .[0] > 0                                            # not at a border/side
          and .[1] > 0
          and .[0] < ($input    | length - 1)
          and .[1] < ($input[0] | length - 1)
          )
  ]
| map( . as [$y, $x]
     | $input
     | [ ([.[$y-1][$x-1], .[$y][$x], .[$y+1][$x+1]] | join(""))  # select diagonal '\'
       , ([.[$y+1][$x-1], .[$y][$x], .[$y-1][$x+1]] | join(""))  # select diagonal '/'
       ]
     | all(. == "MAS" or . == "SAM")                             # check MAS
     | select(.)                                                 # keep successes only
     )
| length                                                         # and count them
