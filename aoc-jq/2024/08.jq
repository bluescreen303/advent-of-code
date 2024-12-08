#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

def pairs:
  if   length < 2 then error("cannot pair")
  elif length > 2 then [.[0]] + (.[1:][] | [.]), (.[1:] | pairs)
  end
;

def anti_a($height; $width):
  def antinode:
    { x: .[1].x + (.[1].x - .[0].x),
      y: .[1].y + (.[1].y - .[0].y),
      value: "#",
    }
  ;
  antinode, ([.[1], .[0]] | antinode)                           # calculate the two antinodes
;

def anti_b($height; $width):                                    # find the (stepwise)line of antinodes
    (.[1].x - .[0].x)             as $step_x
  | (.[0].x % $step_x)            as $start_x
  | ((.[1].y - .[0].y) / $step_x) as $slope
  | (.[0].y - $slope * .[0].x)    as $intercept
  | range($start_x; $width; $step_x | abs)
  | { x: .,
      y: . * $slope + $intercept | round,
      value: "#"
    }
;

utils::parse_grid
| length          as $height
| (.[0] | length) as $width
| [ utils::with_context[][]                                     # give each point in the grid their own X and Y coordinates
  | select(.value != ".")                                       # we don't care about empty (.) points
  ]                                                             # so at this stage, we have a collection of {x, y, value (frequency)}
| group_by(.value)                                              # then per frequency...
| map(pairs)                                                    # we get all possible node pairs
| map(anti_a($height; $width)), map(anti_b($height; $width))    # to which we apply antinode logic (different per puzzle)
| map(select( .x >= 0     and .y >= 0                           # filter to board size
          and .x < $width and .y < $height))
| unique                                                        # nodes can only be an antinode once
| length
