#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

def parse:
  split("\n\n")[]
  | match("Button A:\\s+X\\+?([-0-9]+),\\s+Y\\+?([-0-9]+)\\s+Button B:\\s+X\\+?([-0-9]+),\\s+Y\\+?([-0-9]+)\\s+Prize:\\s+X=([0-9]+),\\s+Y=([0-9]+)\\s*"; "gm")
  | [.captures[].string | tonumber]
;

def presses($ax; $ay; $bx; $by; $px; $py):
  ((($py * $bx) - ($px * $by)) / (($ay * $bx) - ($ax * $by)) | utils::as_int) as $a |
  (($px - ($a * $ax)) / $bx | utils::as_int) as $b |
  {$a, $b}
;

def score: .a * 3 + .b;

[ parse ]
| (., map(.[range(4;6)] += 10000000000000)) # puzzle A, puzzle B
| map(presses(.[0]; .[1]; .[2]; .[3]; .[4]; .[5]) | score)
| add
