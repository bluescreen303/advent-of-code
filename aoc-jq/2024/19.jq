#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

def parse:
  split("\n\n")
  | { patterns: .[0] | split(", "),
      designs:  .[1] | [split("\n")[] | select(. != "")],
    }
;

def match_step($design; $patterns):
  .[0] as $next | .[1:] as $rest |
  [ $patterns[]
    | . as $pattern
    | select($design[$next.pos:] | startswith($pattern))
    | { pos: length + $next.pos, number: $next.number }
  ] + $rest # could be nicer with a merge-sort, as $rest is already sorted
  |
  [ group_by(.pos)[]
    | .[0].pos as $pos
    | map(.number)
    | add
    | {$pos, number: .}
  ]
;

def run_steps($design; $patterns):
  match_step($design; $patterns)
  | if length == 0 then empty
    elif (length == 1 and .[0].pos == ($design | length)) | not
    then run_steps($design; $patterns)
    else .[0].number
    end
;

parse
| . as $p
| [[{pos: 0, number: 1}] | run_steps($p.designs[]; $p.patterns)]
| length, add # puzzle a, puzzle b
