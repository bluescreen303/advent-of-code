#!/usr/bin/env -S jaq -L . -Rsfr

import "aoc-jq/utils" as utils;
import "aoc-jq/bitwise" as bitwise;

def xor($a; $b): bitwise::bitwise_xor($a; $b);

def parse:
  split("\n\n")
  | { registers: .[0] | [ match("Register (.): ([0-9]+)"; "gm").captures
                          | { key:   .[0].string | ascii_downcase,
                              value: .[1].string | tonumber,
                            }
                        ] | from_entries,
      program:   .[1] | [match("([0-9]+),?"; "gm").captures[].string | tonumber],
    }
;

# produce {out, new_state}
def step:
  .program[.ip]   as $opcode  |
  .program[.ip+1] as $operand |

  def combo($operand):
    if   $operand >= 0 and $operand <= 3 then $operand
    elif $operand == 4 then .registers.a
    elif $operand == 5 then .registers.b
    elif $operand == 6 then .registers.c
    else error("invalid combo operand: \($operand)")
    end
  ;

  if $opcode == 3 and .registers.a != 0 then     #jnz
    .ip = $operand
  else
    if   $opcode == 0 then                       # adv
      .registers.a = (.registers.a / pow(2; combo($operand)) | floor)
    elif $opcode == 1 then                       # bxl
      .registers.b |= xor(.; $operand)
    elif $opcode == 2 then                       # bst
      .registers.b = combo($operand) % 8
    elif $opcode == 3 then                       # jnz
      . # do nothing
    elif $opcode == 4 then                       # bxc
      .registers.b = xor(.registers.b; .registers.c)
    elif $opcode == 5 then                       # out
      .out = combo($operand) % 8
    elif $opcode == 6 then                       # bdv
      .registers.b = (.registers.a / pow(2; combo($operand)) | floor)
    elif $opcode == 7 then                       # cdv
      .registers.c = (.registers.a / pow(2; combo($operand)) | floor)
    else error("invalid opcode: \($opcode)")
    end
    | .ip += 2                                   # increase the instruction pointer
  end
  | { out, state: del(.out) }
;

def run:
  step                                           # take a step
  | if .out != null then .out else empty end     # output when there is output
  , ( .state                                     # and continue with current state
    | select(.ip < (.program | length))          # but only when there are more ops
    | run
    )
;

# puzzle b:
#
# This one is a little tricky, and this solution is kind-of a cheat because it will only
# work for our specific puzzle input, not any generic given program.
# It does however run blazingly fast, finding all 6 total solutions in milliseconds.
#
# The logic is derived from (manually) decompiling the program to (pseudo)code:
#
# {
#   b = a % 8;
#   b = b xor 1;
#   c = a / 2 ^ b;
#   b = b xor 5;
#   b = b xor c;
#   print b mod 8;
#   a = a / 2 ^ 3;
# } until(a == 0)
#
# optimized: (math to bit shifts, unfactorization, lifting mod)
# {
#   b = (a % 8) xor 1;
#   print((a % 8) xor ((a >> b) % 8) xor 4);
#   a = a >> 3;
# } until (a == 0)
#
# As our beloved JQ does not have built-in bitwise operators,
# and 3 bit "bytes" would be terrible anyway, unfolding to bit-array logic makes
# all those shifts really tangible:
#
# a = [0, 1, 1, 1, 0, 0, 1, ...] # little endian bit array
# for i = 0; i < a.length; i = i + 3 {
#   b = a[i:i+3] xor 1;
#   print(a[i:i+3] xor a[i+b:i+b+3] xor 4)
# }
#
# This shows the relation between A and the output digits quite clearly.
# Of course the goal is to calculate a by minimal guessing, so basically reversing the logic.
# For the most part walking low-to-high in 3 bit steps provides our numbers.
# But it gets mangled with bits from "higher up", meaning that picking lowest first would need
# us to remember things about future bits while traversing up, which is non-trivial.
#
# So instead of pushing "future restrictions" up-hill, working backwards (downwards)
# prevents that issue (we can see the stuff that already was produced, without re-evaluating
# the rules that produced that). In addition, because we know the program length, it severely
# restricts options for the high numbers (who should deal with all-zeroes "above" them).
# That trickles down nicely as lower digits get restricted by the higher ones already picked.
# In total, it turns out there are only 6 possible choices (3 for one digit, then 2 below).


def reverse_logic:
  first(                                         # only show the first valid A (out of 6)
    reduce reverse[] as $item                    # walk backwards, as lower bits may topple higher ones
    ( []                                         # remainder, already produced bits to the right
    ; . as $rest
      | range(8) as $try                         # try all 8 options per 3bit
      | xor($try; 1) as $try1                    # algorithm uses this one as a shifted view
      | ( $try
        | [bitwise::bitwise] + [0,0,0]           # turn our number into 3 bits, padded to 3
        | .[0:3] + $rest                         # and push them into the remainder
        )
      | select(                                  # check whether applying the algorithm outputs
          xor($try;xor(4; .[$try1:$try1+3]))     # the wanted result (opcode/operand)
          == $item
        )
    ) | bitwise::to_int                          # turn bits array into number again
  )
;

parse
| .ip = 0
| ([run | tostring] | join(","))                 # puzzle a
, (.program | reverse_logic)                     # puzzle b