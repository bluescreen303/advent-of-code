#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

def trailheads: .[][] | select(.value == 0);

def trails($grid):
    ($grid    | length) as $height
  | ($grid[0] | length) as $width
  | . as $me
  | if $me.value == 9 then [$me] else
      ((.y -= 1), (.y +=1 ), (.x -= 1), (.x += 1))
    | select(.y >= 0 and .y < $height and .x >= 0 and .x < $width)
    | $grid[.y][.x]
    | select(.value == $me.value + 1)
    | [$me] + trails($grid)
    end
;

utils::parse_grid
| map(map(tonumber?))                         # fields are numbers, non-numbers (.) become null
| utils::with_context                         # provide fields with {x, y} context
| . as $grid
| [ trailheads                                # find all trailheads
  | [ trails($grid)[-1] ]                     # get their trails, look at the last step (9)
  | unique                                    # score is the number of unique reachable 9ers
  | length
  ]
| add
