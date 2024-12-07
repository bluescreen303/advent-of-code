#!/usr/bin/env -S jaq -Rsf

def one_mul:
  .captures
  | map(.string | tonumber)
  | .[0] * .[1]
;

def step($x):
  if $x.string == "don't()"
    then .capturing = false
  elif $x.string == "do()"
    then .capturing = true
  elif .capturing
    then .sum += ($x | one_mul)
  end
;

# puzzle a
( [ match("mul\\(([0-9]+),([0-9]+)\\)"; "gm")
  | one_mul
  ] | add
),
# puzzle b
( [match("mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"; "gm")]
  | reduce .[] as $x
    ( {capturing: true, sum: 0}
    ; step($x)
    ).sum
)
