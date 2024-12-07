#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

# how to recognize the guard at some location?
def is_guard(loc): select(loc as $v | "^v<>" | contains($v));

# how to recognize a loop, given the incoming direction
def loop_chars:
  if   . == "^" then "^<>"
  elif . == "v" then "v<>"
  elif . == "<" then "^v<"
  elif . == ">" then "^v>"
  else error("Error: unknown dir '\(.)'")
  end
;

# cells from (y, x) in $dir direction
# limited by $max steps or to the end of the map when null
# returns cells/selectors, not just an ordinary list
def look($y; $x; $dir; $max):
  if   $dir == "^" then .[range($y;$y-($max // $y)-1;-1)][$x]
  elif $dir == "v" then .[range($y;$y+($max // (length-$y-1))+1)][$x]
  elif $dir == "<" then .[$y][range($x;$x-($max // $x)-1;-1)]
  elif $dir == ">" then .[$y][range($x;$x+($max // (.[0]|length-$x-1))+1)]
  else error("Error: unknown dir '\($dir)'")
  end
;
def look($y; $x; $dir): look($y; $x; $dir; null);

# guard location is either {x, y, dir}, "out", or "loop"
def new_location($n):
  .field as $field | .guard_location
  | if $n == null  then "out"
    elif .value == "^" then .y -= $n | .value = ">"
    elif .value == "v" then .y += $n | .value = "<"
    elif .value == "<" then .x -= $n | .value = "^"
    elif .value == ">" then .x += $n | .value = "v"
    else error("Error: unknown dir '\(.value)'")
    end
  | first( ( (.value | loop_chars) as $loop_chars          # loop detection
           | $field[.y][.x] as $current
           | select($loop_chars | contains($current))
           | "loop"
           )?
         , .
         )
;

def walk: # (and turn)
  .guard_location as {$y, $x, value: $dir}
  | (.field | [look($y;$x;$dir)][1:] | index("#")) as $n   # how far can we walk in this direction?
  | .guard_location = new_location($n)                     # get there (including rotate)
  | (.field | look($y;$x;$dir;$n)) = $dir                  # mark the path
;

def default_route:
  { field: .,
    guard_location: utils::with_context[][] | is_guard(.value)
  }
  | until( .guard_location | type == "string"; walk)       # keep walking until "out" or "loop"
  | .field
;

def with_one_obstacle:
  range(length) as $y
  | range(.[0] | length) as $x
  | is_guard(.[$y][$x])                                    # only consider options where the guard would walk
  | (.[][] | is_guard(.)) = "."                            # clean out default path
  | .[$y][$x] = "#"                                        # place obstacle
;

# main
utils::parse_grid
| (utils::with_context[][] | is_guard(.value)) as $guard_location
| default_route
| ( utils::show_grid                                       # plot route
  , ""
  , ([.[][] | is_guard(.)] | length)                       # puzzle 06a
  , (                                                      # puzzle 06b
    [ with_one_obstacle
    | { field: ., $guard_location }
    | until( .guard_location | type == "string"; walk)     # keep walking until "out" or "loop"
    | select(.guard_location == "loop")                    # we need "loop" results only
    ]
    | length                                               # count them
    )
  )
