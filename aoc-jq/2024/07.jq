#!/usr/bin/env -S jaq -L . -Rsfr

def parse:
  split("\n")[]
  | select(. != "")
  | [ split("[\\s:]+"; "")[]        # split on whitespace or ':'
    | tonumber
    ]
  | {result: .[0], numbers: .[1:]}
;

def valid(ops):
  .result as $result
  | any( .numbers
       | reduce .[1:][] as $item    # loop over second and remaining numbers
         ( [ .[0] ]                 # start with just the first number
         ; [ .[]                    # try all current possible results
           | [., $item]             # pair with the next number
           | ops                    # and run through the operations
           ]
           | unique
         )[]
       ; . == $result               # does any of the produced totals match?
       )
;

def main(ops):
  [ parse
  | select(valid(ops)).result
  ]
  | add
;

# puzzle 07a
main( .[0] + .[1]
    , .[0] * .[1]
    )
,
# puzzle 07b
main( .[0] + .[1]
    , .[0] * .[1]
    , "\(.[0])\(.[1])" | tonumber
    )
