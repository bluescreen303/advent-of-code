#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;

def path_cost:
  .direction as $dir

  # we can continue or turn 90 degrees
  | if   .direction == "^" or .direction == "v"
    then .direction = (.direction, "<", ">")
    else .direction = (.direction, "^", "v")
    end

  # then step in that direction
  | if   .direction == "^" then (.y -= 1)
    elif .direction == "v" then (.y += 1)
    elif .direction == "<" then (.x -= 1)
    elif .direction == ">" then (.x += 1)
    else error("unknown direction: \(.direction)")
    end

  # turning costs 1000 extra
  | .score += if .direction == $dir then 1 else 1001 end
;


def progress($field):
  path_cost                                           # all possible moves and their cost
  | $field[.y][.x] as $target                         # look at that spot in the real world
  | select($target.value != "#"                       # don't bump into walls
       and $target.score[.direction] == null)         # go where noone has gone before
;

def race($field):
    group_by(.score) | [.[1:][][]] as $rest | .[0]    # deal with best paths only, keep rest
  | map(progress($field))                             # progress them
  | reduce (.[]) as $path                             # update the field
    ( $field
    ; .[$path.y][$path.x].score[$path.direction] = $path.score
    ) as $field
  | utils::partition($field[.y][.x].value == "E") as [$finishers, $progressors]

  | ($finishers[] | {finisher: ., $field})            # report paths that finished
  , ( $progressors + $rest                            # continue with the rest
    | sort_by(.score) | unique_by(.x, .y, .direction) # kill paths that merge
    | select(. != []) | race($field)                  # recurse while there are paths left
    )
;

def step_back($field):
  [ .[] | . as $me                                    # for each backtrace
    | .history // [] as $history                      # look up where we came from
    | .score                                          # then based on tile entry scores
    | [ to_entries[]
      | if [.key] - $history != [] then               # if we had to take a turn
          .value += 1000                              # that cost us
        end
      ]
    | group_by(.value)[0][].key as $dir               # only explore neighbouring tiles with best score, remember the direction
    | if   $dir == "^" then $me | .y+=1
      elif $dir == "v" then $me | .y-=1
      elif $dir == "<" then $me | .x+=1
      elif $dir == ">" then $me | .x-=1
      else error("unknown score key: \($dir)")
      end
    | $field[.y][.x]                                  # go there
    | .history = [$dir]                               # and register where we came from
  ]
  | [ group_by(.x, .y)[]                              # then combine/prune backtraces that landed on same tile
      | (.[0].history = [.[].history[]])[0]           # and merge their histories
    ]
;

def tiles_visited($field):
  # all entry points have been visited
  .[],

  # then we recurse
  ( [ .[] | select(.value != "S") ]                   # we're done when we reach Start
  | select(. != [])                                   # out of paths to explore
  | step_back($field)                                 # all remaining paths reason one step backwards
  | tiles_visited($field)
  )
;

utils::parse_grid                                     # get the grid
| utils::with_context                                 # add x, y, value semantics
| .[][].score = {} as $field                          # give each tile a scoring table (per inbound direction)

| first([ .[][]                                       # we need the best (first) paths only
        | select(.value == "S")                       # start at S
        | .direction = ">"                            # start direction east
        | .score = 0                                  # start score
        ] | race($field))                             # and GO!
  as {$finisher, $field}

| $finisher.score                                     # puzzle a

, ( [$field[][] | select(.value == "E") ]             # puzzle b walks backwards from E
    | [tiles_visited($field)]
    | length                                          # and counts them
  )
