#!/usr/bin/env -S jaq -Rsf

def parse:
  split("\n")[]
  | select(. != "")       # drop empty line
  | [ split("\\s+"; "")[] # split by 1 or more whitespace
    | tonumber
    ]
;

[parse] |
transpose |

# puzzle a
( map(sort)
| transpose
| map(.[0] - .[1] | abs)
| add
),

# puzzle b
( ( .[1]
  | group_by(.)
  | map({key: .[0] | tostring, value: length})
  | from_entries
  ) as $right
| .[0]
| map(. * ($right[tostring] // 0))
| add
)
