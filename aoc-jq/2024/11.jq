#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

def parse:
  split("\n")[]
  | select(. != "")
  | split(" ")[]
  | tonumber
;

def blink:
  reduce to_entries[] as {key: $stone, value: $count}
  ( {}
  ; ($stone | length) as $digits
  | .[ $stone
    |  tonumber
    |  if . == 0 then 1
       elif $digits % 2 == 0 then
         ($digits/2|floor) as $x
         | ($stone[0:$x] | tonumber)
         , ($stone[$x:$digits] | utils::drop_leading_0s | tonumber)
       else . * 2024
       end
    |  tostring
    ] += $count
  )
;

def blink($n): reduce range($n) as $i(.; blink);

reduce parse as $stone({}; .[$stone|tostring] += 1) # turn into counted map
| blink(25, 75)                                     # puzzle a, puzzle b
| add
