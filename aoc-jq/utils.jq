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
