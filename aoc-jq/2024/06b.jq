#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;
include "aoc-jq/2024/06-shared";

def with_one_obstacle:
  range(length) as $y
  | range(.[0] | length) as $x
  | is_guard(.[$y][$x])         # only consider options where the guard would walk
  | (.[][] | is_guard(.)) = "." # clean out default path
  | .[$y][$x] = "#"             # place obstacle
;

# main
utils::parse_grid
| (utils::with_context[][] | is_guard(.value)) as $guard_location
| default_route
| [ with_one_obstacle
  | { field: ., $guard_location }
  | until(.guard_location | type == "string"; step)
  | select(.guard_location == "loop")
  ]
| length
