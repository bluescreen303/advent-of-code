def with_context:
  to_entries
  | map(
      .key as $y
      | .value
      | to_entries
      | map({x: .key, $y, value})
    )
;

def parse_grid:
  split("\n")
  | map(
      select(. != "")
      | split("")
    )
;

def show_grid:
  map(join(""))
  | join("\n")
;

# (a -> bool): [a] -> [[a], [a]]
# partition a list between items for which a predicate holds
# and those for which it doesn't
def partition(pred):
  reduce .[] as $item
  ( [[], []]
  ; .[if $item | pred then 0 else 1 end] += [$item]
  )
;

# (a -> bool): [a] -> [[a], [a]]
# split a list at the point where the predicate holds
# also known as 'break'
def break_when(pred):
  def go:
    if length == 0 then [[], []]
    else
      .[0] as $item
      | if $item | pred
        then [[], .]
        else .[1:] | go | (.[0] |= [$item] + .)
        end
    end
  ;
  go
;
