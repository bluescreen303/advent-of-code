#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

def trailheads: .[][] | select(.value == 0);

def trails($grid):
    ($grid    | length) as $height
  | ($grid[0] | length) as $width
  | . as $me
  | [$me]
  + if $me.value == 9 then [] else
      ((.y -= 1), (.y +=1 ), (.x -= 1), (.x += 1))
    | select(.y >= 0 and .y < $height and .x >= 0 and .x < $width)
    | $grid[.y][.x]
    | select(.value == $me.value + 1)
    | trails($grid)
    end
;

def score:  map(.[-1]) | unique | length;     # score cares about unique reachable tops (9)
def rating: length;                           # rating cares about distinct paths

utils::parse_grid
| map(map(tonumber?))                         # fields are numbers, non-numbers (.) become null
| utils::with_context                         # provide fields with {x, y} context
| . as $grid
| [ trailheads | [trails($grid)] ]            # find all trails
| map(score), map(rating)                     # get their scores (puzzle a) and ratings (puzzle b)
| add
