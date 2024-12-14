#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

101 as $width | 103 as $height |

def parse:
  split("\n")[]
  | match("p=(?<px>[0-9]+),(?<py>[0-9]+) v=(?<vx>[-0-9]+),(?<vy>[-0-9]+)")
  | [ .captures[] | {key: .name, value: .string | tonumber} ]
  |from_entries
;

def move($seconds):
    .px = (.px + $seconds * .vx | utils::rem($width))
  | .py = (.py + $seconds * .vy | utils::rem($height))
;

($width  / 2 | floor) as $h | ($height / 2 | floor) as $v |
def in_quad(q): map(select(q)) | length;
def top_left:     .px < $h and .py < $v;
def top_right:    .px > $h and .py < $v;
def bottom_left:  .px < $h and .py > $v;
def bottom_right: .px > $h and .py > $v;

def puzzle_a:
  [.[] | move(100)]
  | reduce ( in_quad(top_left), in_quad(top_right)
           , in_quad(bottom_left), in_quad(bottom_right)
           ) as $q (1; . * $q)
;

def puzzle_b:
  . as $start |
  0 | until( . as $i
           | [$start[] | move($i)]
           | group_by(.px, .py)
           | all(length == 1)
           ; . += 1
           )
;

[ parse ] | puzzle_a, puzzle_b
