import "aoc-jq/utils" as utils;

def is_guard(loc): select(loc as $v | "^v<>" | contains($v));

def look($y; $x; $dir; $max):
  if   $dir == "^" then .[range($y;$y-($max // $y)-1;-1)][$x]
  elif $dir == "v" then .[range($y;$y+($max // (length-$y-1))+1)][$x]
  elif $dir == "<" then .[$y][range($x;$x-($max // $x)-1;-1)]
  elif $dir == ">" then .[$y][range($x;$x+($max // (.[0]|length-$x-1))+1)]
  else error("Error: unknown dir '\($dir)'")
  end
;
def look($y; $x; $dir): look($y; $x; $dir; null);

def loopers:
  if   . == "^" then "^<>"
  elif . == "v" then "v<>"
  elif . == "<" then "^v<"
  elif . == ">" then "^v>"
  else error("Error: unknown dir '\(.)'")
  end
;

def new_location($n):
  .field as $field | .guard_location
  | if $n == null  then "out"
    elif .value == "^" then .y -= $n | .value = ">"
    elif .value == "v" then .y += $n | .value = "<"
    elif .value == "<" then .x -= $n | .value = "^"
    elif .value == ">" then .x += $n | .value = "v"
    else error("Error: unknown dir '\(.value)'")
    end
  | first( ( (.value | loopers) as $loopers                  # loop detection
           | $field[.y][.x] as $now
           | select($loopers | contains($now))
           | "loop"
           )?
         , .
         )
;

def step:
  .guard_location as {$y, $x, value: $dir}
  | (.field | [look($y;$x;$dir)][1:] | index("#")) as $n     # how far can we walk in this direction?
  | .guard_location = new_location($n)
  | (.field |  look($y;$x;$dir;$n)) = $dir                   # mark those paths
;

def default_route:
  { field: ., guard_location: utils::with_context[][] | is_guard(.value) }
  | until(.guard_location | type == "string"; step)
  | .field
;
