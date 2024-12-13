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

def diagonal($b):
    .[0] as $e | .[1:] as $es
  | ($b | map(.[1:]))  as $ts
  | [$b | map(.[0])]
    + if $e == null
      then $ts | transpose | map(map(select(. != null)))
      else $es | diagonal([$e] + $ts)
      end
;

def diagonal: # '/'
  diagonal([])[1:]
;

def diagonal2:    # '\'
  map(reverse) | diagonal | map(reverse)
;

def toposort($root; $edges):
  def go($node; $visited):
    reduce ($edges[$node | tostring] // [])[] as $child
      ( {$visited, result: []}
      ; if (.visited | any(. == $child)) | not then
          (.visited, .result) += go($child; .visited + [$child])
        end
      ).result + [$node]
  ;
  go($root; [$root]) | reverse
;

def min($x; $y):
  if $y < $x then $y else $x end
;

def max($x; $y):
  if $x > $y then $x else $y end
;

def as_int:
  select(. == (. | floor)) | floor
;

def drop_leading_0s:
  sub("0*(?<x>[0-9])";"\(.x)")
;
