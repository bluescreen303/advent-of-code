#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

def parse:
  split("\n")
  | index("") as $i                                  # find the empty line
  | { edges: [ .[0:$i][]
             | [split("|")[] | tonumber]
             ]
             | reduce .[] as [$from, $to]
               ( {}                                  # create a dictionary
               ; .[$from | tostring]                 # use 'from' as key
                 |= (. // []) + [$to]                # and a list of found 'to's as the value
               ),
      updates: [ .[$i+1:][]
               | select(. != "")
               | [split(",")[] | tonumber]
               ],
    }
;

def correct($edges):
  def go:
    reduce .[] as $item
    ( []
    ; $edges[$item|tostring] // [] as $rules         # check the rules for the to-be-added item
      | utils::break_when( . as $item                # split at the first violation
                         | any($rules[]; . == $item)
                         ) as [$good, $bad]
      | $good + if $bad | length > 0
                then [$item] + $bad | go             # violations, place item before first violation and re-correct
                else [$item]                         # no violations, just add the item
                end
    )
  ;
  go
;

parse
| .edges as $edges
| [.updates[] | [., correct($edges)]]                # pair each update with its corrected version
| utils::partition(.[0] == .[1])                     # divide between correct/incorrect (puzzle 05a and 05b)
| .[]                                                # for both puzzles
| [.[][1] | .[length / 2 | floor]]                   # take the middle item from the corrected version
| add                                                # and sum the result
